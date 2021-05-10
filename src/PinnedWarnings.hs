{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module PinnedWarnings
  ( plugin
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)
import           Data.IORef
import           Data.List
import           Data.Monoid (Alt(..))
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.String (fromString)
import           Data.Time
import qualified System.Directory as Dir
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.ParserCombinators.ReadP as P

import qualified GhcFacade as Ghc

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type ModuleFile = Ghc.FastString

newtype Warning = Warning { unWarning :: Ghc.WarnMsg }

instance Eq Warning where
  Warning a == Warning b = show a == show b

instance Ord Warning where
  compare (Warning a) (Warning b) = compare (show a) (show b)

newtype MonoidMap k a = MonoidMap (M.Map k a)
  deriving Foldable

instance (Ord k, Semigroup a) => Semigroup (MonoidMap k a) where
  MonoidMap a <> MonoidMap b = MonoidMap $ M.unionWith (<>) a b

instance (Ord k, Semigroup a) => Monoid (MonoidMap k a) where
  mempty = MonoidMap M.empty

type SrcSpanKey = (Ghc.RealSrcLoc, Ghc.RealSrcLoc)

type WarningsWithModDate =
  ( Alt Maybe UTCTime -- Last time the module was modified
  , MonoidMap SrcSpanKey (S.Set Warning)
  )

-- The infamous mutable global trick.
-- Needed to track the pinned warnings during and after compilation.
globalState :: MVar (M.Map ModuleFile WarningsWithModDate)
globalState = unsafePerformIO $ newMVar mempty
{-# NOINLINE globalState #-}

--------------------------------------------------------------------------------
-- Plugin
--------------------------------------------------------------------------------

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.tcPlugin = const $ Just tcPlugin
    , Ghc.parsedResultAction = const resetPinnedWarnsForMod
    , Ghc.dynflagsPlugin = const addWarningCapture
    }

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit  = initTcPlugin
    , Ghc.tcPluginSolve = \(sw, fw, counterRef) _ _ wanteds ->
        checkWanteds sw fw counterRef wanteds
    , Ghc.tcPluginStop  = const $ pure ()
    }

initTcPlugin :: Ghc.TcPluginM (Ghc.TyCon, Ghc.TyCon, IORef Int)
initTcPlugin =
  (,,) <$> lookupShowWarnings
       <*> lookupFixWarnings
       <*> Ghc.tcPluginIO (newIORef 0)

-- | Gets a reference to the 'ShowWarnings' constraint
lookupShowWarnings :: Ghc.TcPluginM Ghc.TyCon
lookupShowWarnings = do
  result <- Ghc.findImportedModule
              (Ghc.mkModuleName "ShowWarnings")
              (Just  "pinned-warnings")

  case result of
    Ghc.Found _ mod' -> do
      name <- Ghc.lookupOrig mod' $ Ghc.mkTcOcc "ShowWarnings"
      Ghc.classTyCon <$> Ghc.tcLookupClass name

    _ -> error "ShowWarnings module not found"

-- | Gets a reference to the 'FixWarnings' constraint
lookupFixWarnings :: Ghc.TcPluginM Ghc.TyCon
lookupFixWarnings = do
  result <- Ghc.findImportedModule
              (Ghc.mkModuleName "ShowWarnings")
              (Just  "pinned-warnings")

  case result of
    Ghc.Found _ mod' -> do
      name <- Ghc.lookupOrig mod' $ Ghc.mkTcOcc "FixWarnings"
      Ghc.classTyCon <$> Ghc.tcLookupClass name

    _ -> error "ShowWarnings module not found"

-- | If any wanted constraints are for 'ShowWarnings', then inject the pinned
-- warnings into GHC.
checkWanteds :: Ghc.TyCon
             -> Ghc.TyCon
             -> IORef Int
             -> [Ghc.Ct]
             -> Ghc.TcPluginM Ghc.TcPluginResult
checkWanteds sw fw counterRef
    = fmap (flip Ghc.TcPluginOk [] . catMaybes)
    . traverse go
  where
    go ct@Ghc.CDictCan { Ghc.cc_class = cls }
      | Ghc.classTyCon cls == sw = do
          counter <- Ghc.tcPluginIO $ readIORef counterRef

          -- for some reason warnings only appear if they are added on
          -- particular iterations.
          when (counter == 2) addWarningsToContext
          incrementCounter

          pure $ Just (Ghc.EvExpr Ghc.unitExpr, ct)

      | Ghc.classTyCon cls == fw = do
          counter <- Ghc.tcPluginIO $ readIORef counterRef

          when (counter == 0) (Ghc.tcPluginIO fixWarnings)
          incrementCounter

          pure $ Just (Ghc.EvExpr Ghc.unitExpr, ct)

    go _ = pure Nothing

    incrementCounter =
      Ghc.tcPluginIO $ modifyIORef' counterRef succ

-- | Add warnings from the global state back into the GHC context
addWarningsToContext :: Ghc.TcPluginM ()
addWarningsToContext = do
  errsRef <- Ghc.tcl_errs . snd <$> Ghc.getEnvs

  Ghc.tcPluginIO pruneDeleted
  pinnedWarns <- Ghc.listToBag . map unWarning
               . foldMap (foldMap S.toList . snd)
             <$> Ghc.tcPluginIO (readMVar globalState)

  Ghc.tcPluginIO . atomicModifyIORef' errsRef
    $ \(warnings, errors) ->
        ((Ghc.unionBags pinnedWarns warnings, errors), ())

-- | Remove warnings for modules that no longer exist
pruneDeleted :: IO ()
pruneDeleted = modifyMVar_ globalState $ \warns -> do
  let mods = M.keys warns

  deletedMods <-
    filterM (fmap not . Dir.doesFileExist . Ghc.unpackFS)
            mods

  pure $ foldl' (flip M.delete) warns deletedMods

-- | Removes currently pinned warnings for a module and updates the timestamp.
-- This occurs before any new warnings are captured for the module.
resetPinnedWarnsForMod
  :: Ghc.ModSummary
  -> Ghc.HsParsedModule
  -> Ghc.Hsc Ghc.HsParsedModule
resetPinnedWarnsForMod modSummary parsedModule = do
  let modFile = fromString $ Ghc.ms_hspp_file modSummary
      modifiedTime = Alt . Just $ Ghc.ms_hs_date modSummary

  -- Replace any existing pinned warnings with new ones for this module
  liftIO . modifyMVar_ globalState
    $ pure . M.insert modFile (modifiedTime, mempty)

  pure parsedModule

-- | Taps into the log action to capture the warnings that GHC emits.
addWarningCapture :: Ghc.DynFlags -> IO Ghc.DynFlags
addWarningCapture dynFlags = do
  pure dynFlags
    { Ghc.log_action = Ghc.log_action' (Ghc.log_action dynFlags) $
      \dyn severity srcSpan msgDoc -> do
        case (severity, Ghc.srcSpanFileName_maybe srcSpan) of
          (Ghc.SevWarning, Just modFile)
            | Ghc.RealSrcLoc' start <- Ghc.srcSpanStart srcSpan
            , Ghc.RealSrcLoc' end <- Ghc.srcSpanEnd srcSpan
            -> do
              let warn = M.singleton (start, end)
                       . S.singleton
                       . Warning
                       $ Ghc.mkWarnMsg dyn srcSpan Ghc.alwaysQualify msgDoc

              modifyMVar_ globalState
                $ pure
                . M.insertWith (<>) modFile
                    (mempty, MonoidMap warn)

          _ -> pure ()
    }

fixWarnings :: IO ()
fixWarnings = do
  pruneDeleted
  modifyMVar_ globalState $ traverse fixWarning

-- | Fixes applicable warning and returns 'False' if all warnings for the
-- corresponding span should be removed.
fixWarning :: WarningsWithModDate -> IO WarningsWithModDate
fixWarning (Alt (Just modifiedAt), MonoidMap warnMap) = do
  (pairs, files) <- (`runStateT` M.empty)
           . flip filterM (reverse $ M.toList warnMap) $ \case
    ((start, end), warnSet)
      | Alt (Just reWarn)
          <- foldMap (Alt . parseRedundancyWarn) warnSet
      -> do
            let file = Ghc.unpackFS $ Ghc.srcLocFile start

            mCached <- gets (M.lookup file)

            srcLines <-
              maybe (liftIO . fmap BS.lines $ BS.readFile file)
                    pure
                    mCached

            mNewSrcLines <- liftIO $
              fixRedundancyWarning start end modifiedAt reWarn srcLines

            case mNewSrcLines of
              Nothing -> pure True

              Just newSrcLines -> do
                modify' $ M.insert file newSrcLines

                pure False

    _ -> pure True

  _ <- M.traverseWithKey
         (\file ls -> do
           BS.writeFile file $ BS.unlines ls
           putStrLn $ "'" <> file <> "' has been edited"
         )
         files

  pure (Alt Nothing, MonoidMap $ M.fromList pairs)

fixWarning w = pure w

--------------------------------------------------------------------------------
-- Redundant import warnings
--------------------------------------------------------------------------------

-- | Redundant import warnings
data RedundancyWarn
  = WholeModule
  | IndividualThings [String]
  deriving Show

-- | Attempt to fix redundant import warning.
fixRedundancyWarning :: Ghc.RealSrcLoc
                     -> Ghc.RealSrcLoc
                     -> UTCTime
                     -> RedundancyWarn
                     -> [BS.ByteString]
                     -> IO (Maybe [BS.ByteString])
fixRedundancyWarning start end lastModified warn srcLines = do
  let file = Ghc.unpackFS $ Ghc.srcLocFile start

  fileModified <- Dir.getModificationTime file

  if fileModified /= lastModified
     then do
       putStrLn $ "'" <> file <> "' has been modified since warnings were last collected."
       pure Nothing

     else
       case warn of
         WholeModule -> do
           let startLine = Ghc.srcLocLine start
               endLine = Ghc.srcLocLine end
               (before, rest) = splitAt (startLine - 1) srcLines
               (_, after) = splitAt (endLine - startLine + 1) rest

           pure . Just $ before <> after

         -- TODO
         IndividualThings _things -> pure Nothing

parseRedundancyWarn :: Warning -> Maybe RedundancyWarn
parseRedundancyWarn (Warning warn) =
  case P.readP_to_S redundancyWarnParser (show warn) of
    [(w, "")] -> Just w
    _ -> Nothing

redundancyWarnParser :: P.ReadP RedundancyWarn
redundancyWarnParser = do
  _ <- P.string "The import of ‘"

  inQuotes <-
    P.sepBy1 (P.munch1 $ \c -> not (isSpace c) && c /= ',' && c /= '’')
             (P.char ',' <* P.skipSpaces)

  _ <- P.char '’'

  let terms
        = IndividualThings inQuotes
         <$ ( P.skipSpaces
           *> P.string "from module ‘"
           *> P.munch1 (/= '’')
           *> P.string "’ is redundant"
            )

      wholeMod = WholeModule <$ (P.skipSpaces *> P.string "is redundant")

  result <- P.choice [terms, wholeMod]

  _ <- P.munch (const True)

  pure result

