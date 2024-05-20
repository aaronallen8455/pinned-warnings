{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module PinnedWarnings
  ( plugin
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
#if !MIN_VERSION_ghc(9,10,0)
import           Data.List
#endif
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.String (fromString)
import qualified System.Directory as Dir
import           System.IO.Unsafe (unsafePerformIO)

import qualified Internal.FixWarnings as FW
import qualified Internal.GhcFacade as Ghc
import           Internal.Types

-- | A mutable global variable used to track warnings during and after
-- compilations.
globalState :: MVar (M.Map ModuleFile WarningsWithModDate)
globalState = unsafePerformIO $ newMVar mempty
{-# NOINLINE globalState #-}

--------------------------------------------------------------------------------
-- Plugin
--------------------------------------------------------------------------------

-- dynFlagsPlugin is being removed in future GHC. There is instead a way to
-- modify the HscEnv and there is a Logger type on HscEnv that should allow
-- for hooking into messages.
plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.tcPlugin           = const $ Just tcPlugin
    , Ghc.parsedResultAction = const resetPinnedWarnsForMod
    , Ghc.driverPlugin       = const (pure . addWarningCapture)
    , Ghc.pluginRecompile    = Ghc.purePlugin
    }

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit  = initTcPlugin
    , Ghc.tcPluginSolve = \pluginState _ _ -> checkWanteds pluginState
    , Ghc.tcPluginStop  = const $ pure ()
#if MIN_VERSION_ghc(9,4,0)
    , Ghc.tcPluginRewrite = mempty
#endif
    }

data PluginState =
  MkPluginState
    { showWarningsClass  :: Ghc.TyCon
    , fixWarningsClass   :: Ghc.TyCon
    , clearWarningsClass :: Ghc.TyCon
    , counterRef         :: IORef Int
    }

initTcPlugin :: Ghc.TcPluginM PluginState
initTcPlugin =
  MkPluginState
    <$> lookupClass "ShowWarnings"
    <*> lookupClass "FixWarnings"
    <*> lookupClass "ClearWarnings"
    <*> Ghc.tcPluginIO (newIORef 0)

-- | Get a reference to a class from the @ShowWarnings@ module
lookupClass :: String -> Ghc.TcPluginM Ghc.TyCon
lookupClass className = do
  result <- Ghc.findImportedModule
              (Ghc.mkModuleName "ShowWarnings")
#if MIN_VERSION_ghc(9,4,0)
              Ghc.NoPkgQual
#else
              (Just "pinned-warnings")
#endif

  case result of
    Ghc.Found _ mod' -> do
      name <- Ghc.lookupOrig mod' $ Ghc.mkTcOcc className
      Ghc.classTyCon <$> Ghc.tcLookupClass name

    _ -> error "ShowWarnings module not found"

-- | If any wanted constraints are for 'ShowWarnings', then inject the pinned
-- warnings into GHC.
checkWanteds :: PluginState
             -> [Ghc.Ct]
             -> Ghc.TcPluginM Ghc.TcPluginResult'
checkWanteds pluginState
    = fmap (flip Ghc.TcPluginOk [] . catMaybes)
    . traverse go
  where
    go ct@(Ghc.CDictCan' _ cls _)
      | Ghc.classTyCon cls == showWarningsClass pluginState = do
          counter <- Ghc.tcPluginIO $ readIORef (counterRef pluginState)

          -- for some reason warnings only appear if they are added on
          -- particular iterations.
          when (counter == 2) addWarningsToContext
          incrementCounter

          pure $ Just (Ghc.EvExpr Ghc.unitExpr, ct)

      | Ghc.classTyCon cls == fixWarningsClass pluginState = do
          counter <- Ghc.tcPluginIO $ readIORef (counterRef pluginState)

          when (counter == 0) (Ghc.tcPluginIO fixWarnings)
          incrementCounter

          pure $ Just (Ghc.EvExpr Ghc.unitExpr, ct)

      | Ghc.classTyCon cls == clearWarningsClass pluginState = do
          counter <- Ghc.tcPluginIO $ readIORef (counterRef pluginState)

          when (counter == 0) (Ghc.tcPluginIO clearWarnings)
          incrementCounter

          pure $ Just (Ghc.EvExpr Ghc.unitExpr, ct)

    go _ = pure Nothing

    incrementCounter =
      Ghc.tcPluginIO $ modifyIORef' (counterRef pluginState) succ

-- | Add warnings from the global state back into the GHC context
addWarningsToContext :: Ghc.TcPluginM ()
addWarningsToContext = do
  errsRef <- Ghc.tcl_errs . snd <$> Ghc.getEnvs

  Ghc.tcPluginIO pruneDeleted
  pinnedWarns <- Ghc.listToBag . map unWarning
               . foldMap (foldMap S.toList . warningsMap)
             <$> Ghc.tcPluginIO (readMVar globalState)

  Ghc.tcPluginIO . atomicModifyIORef' errsRef
#if MIN_VERSION_ghc(9,6,0)
    $ \messages ->
        (Ghc.mkMessages ((fmap . fmap) Ghc.mkTcRnUnknownMessage pinnedWarns)
          `Ghc.unionMessages` messages, ())
#elif MIN_VERSION_ghc(9,4,0)
    $ \messages ->
        (Ghc.mkMessages ((fmap . fmap) Ghc.TcRnUnknownMessage pinnedWarns)
          `Ghc.unionMessages` messages, ())
#endif

-- | Remove warnings for modules that no longer exist
pruneDeleted :: IO ()
pruneDeleted = modifyMVar_ globalState $ \warns -> do
  -- remove keys that have no warnings
  let warns' = M.filter (not . null . warningsMap) warns
      mods = M.keys warns'

  deletedMods <-
    filterM (fmap not . Dir.doesFileExist)
            mods

  pure $ foldl' (flip M.delete) warns' deletedMods

-- | Removes currently pinned warnings for a module and updates the timestamp.
-- This occurs before any new warnings are captured for the module.
resetPinnedWarnsForMod
  :: Ghc.ModSummary
#if MIN_VERSION_ghc(9,4,0)
  -> Ghc.ParsedResult
  -> Ghc.Hsc Ghc.ParsedResult
#else
  -> Ghc.HsParsedModule
  -> Ghc.Hsc Ghc.HsParsedModule
#endif
resetPinnedWarnsForMod modSummary parsedModule = do
  let modFile = fromString $ Ghc.ms_hspp_file modSummary

  liftIO . modifyMVar_ globalState
    $ pure . M.delete modFile

  pure parsedModule

-- | Taps into the log action to capture the warnings that GHC emits.
addWarningCapture :: Ghc.HscEnv -> Ghc.HscEnv
addWarningCapture hscEnv =
  hscEnv
    { Ghc.hsc_logger = Ghc.pushLogHook warningsHook (Ghc.hsc_logger hscEnv)
    }
  where
    warningsHook :: Ghc.LogAction -> Ghc.LogAction
    warningsHook logAction dynFlags messageClass srcSpan sdoc = do
      case messageClass of
#if MIN_VERSION_ghc(9,6,0)
        Ghc.MCDiagnostic Ghc.SevWarning _ _
#else
        Ghc.MCDiagnostic Ghc.SevWarning _
#endif
          | Ghc.RealSrcLoc start _ <- Ghc.srcSpanStart srcSpan
          , Ghc.RealSrcLoc end _ <- Ghc.srcSpanEnd srcSpan
          , Just modFile <- Ghc.srcSpanFileName_maybe srcSpan
          -> do
            let diag =
                  Ghc.DiagnosticMessage
                    { Ghc.diagMessage = Ghc.mkSimpleDecorated sdoc
                    , Ghc.diagReason = Ghc.WarningWithoutFlag
                    , Ghc.diagHints = []
                    }
                diagOpts = Ghc.initDiagOpts $ Ghc.hsc_dflags hscEnv
                warn = Warning $
                  Ghc.mkMsgEnvelope diagOpts srcSpan Ghc.neverQualify diag
            addWarningToGlobalState start end modFile warn
        _ -> pure ()
      logAction dynFlags messageClass srcSpan sdoc

-- | Adds a warning to the global state variable
addWarningToGlobalState
  :: Ghc.RealSrcLoc -- ^ start location
  -> Ghc.RealSrcLoc -- ^ end location
  -> Ghc.FastString -- ^ module name
  -> Warning
  -> IO ()
addWarningToGlobalState start end modFile warn = do
  let wrappedWarn = M.singleton (start, end)
                  $ S.singleton warn
      file = Ghc.unpackFS modFile
  exists <- Dir.doesFileExist file
  when exists $ do
    fileModifiedAt <- Dir.getModificationTime file
    modifyMVar_ globalState
      $ pure
      . M.insertWith (<>) file
          MkWarningsWithModDate
            { lastUpdated = fileModifiedAt
            , warningsMap = MonoidMap wrappedWarn
            }

fixWarnings :: IO ()
fixWarnings = do
  pruneDeleted

  modifyMVar_ globalState $
    M.traverseWithKey FW.fixWarning

clearWarnings :: IO ()
clearWarnings =
  void $ swapMVar globalState M.empty
