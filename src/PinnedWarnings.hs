{-# LANGUAGE OverloadedStrings #-}
module PinnedWarnings
  ( plugin
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List
import           Data.Monoid (Alt(..))
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

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.tcPlugin           = const $ Just tcPlugin
    , Ghc.parsedResultAction = const resetPinnedWarnsForMod
    , Ghc.dynflagsPlugin     = const (pure . addWarningCapture)
    , Ghc.pluginRecompile    = Ghc.purePlugin
    }

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit  = initTcPlugin
    , Ghc.tcPluginSolve = \pluginState _ _ -> checkWanteds pluginState
    , Ghc.tcPluginStop  = const $ pure ()
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
              (Just "pinned-warnings")

  case result of
    Ghc.Found _ mod' -> do
      name <- Ghc.lookupOrig mod' $ Ghc.mkTcOcc className
      Ghc.classTyCon <$> Ghc.tcLookupClass name

    _ -> error "ShowWarnings module not found"

-- | If any wanted constraints are for 'ShowWarnings', then inject the pinned
-- warnings into GHC.
checkWanteds :: PluginState
             -> [Ghc.Ct]
             -> Ghc.TcPluginM Ghc.TcPluginResult
checkWanteds pluginState
    = fmap (flip Ghc.TcPluginOk [] . catMaybes)
    . traverse go
  where
    go ct@Ghc.CDictCan { Ghc.cc_class = cls }
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
addWarningCapture :: Ghc.DynFlags -> Ghc.DynFlags
addWarningCapture dynFlags = do
  dynFlags
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
  modifyMVar_ globalState $ traverse FW.fixWarning

clearWarnings :: IO ()
clearWarnings =
  void $ swapMVar globalState M.empty
