{-# LANGUAGE OverloadedStrings #-}
module PinnedWarnings
  ( plugin
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified System.Directory as Dir
import           System.IO.Unsafe (unsafePerformIO)

import qualified GhcFacade as Ghc

type ModuleFile = BS.ByteString

-- The infamous mutable global trick.
-- Needed to track the pinned warnings during and after compilation.
globalState :: MVar (M.Map ModuleFile Ghc.WarningMessages)
globalState = unsafePerformIO $ newMVar mempty
{-# NOINLINE globalState #-}

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.tcPlugin = const $ Just tcPlugin
    , Ghc.typeCheckResultAction = const insertModuleWarnings
    }

tcPlugin :: Ghc.TcPlugin
tcPlugin =
  Ghc.TcPlugin
    { Ghc.tcPluginInit  = initTcPlugin
    , Ghc.tcPluginSolve = \(sw, counterRef) _ _ wanteds ->
        checkWanteds sw counterRef wanteds
    , Ghc.tcPluginStop  = const $ pure ()
    }

initTcPlugin :: Ghc.TcPluginM (Ghc.TyCon, IORef Int)
initTcPlugin =
  (,) <$> lookupShowWarnings
      <*> Ghc.tcPluginIO (newIORef 0)

-- | Gets a reference to the 'ShowWarnings' constraint
lookupShowWarnings :: Ghc.TcPluginM Ghc.TyCon
lookupShowWarnings = do
  result <- Ghc.findImportedModule
              (Ghc.mkModuleName "ShowWarnings")
              (Just  "pinned-warnings")

  case result of
    Ghc.Found _ mod -> do
      name <- Ghc.lookupOrig mod $ Ghc.mkTcOcc "ShowWarnings"
      Ghc.classTyCon <$> Ghc.tcLookupClass name

    _ -> error "ShowWarnings module not found"

-- | If any wanted constraints are for 'ShowWarnings', then inject the pinned
-- warnings into GHC.
checkWanteds :: Ghc.TyCon
             -> IORef Int
             -> [Ghc.Ct]
             -> Ghc.TcPluginM Ghc.TcPluginResult
checkWanteds sw counterRef
    = fmap (flip Ghc.TcPluginOk [] . catMaybes)
    . traverse go
  where
    go ct@Ghc.CDictCan { Ghc.cc_class = cls }
      | Ghc.classTyCon cls == sw = do
          counter <- Ghc.tcPluginIO $ readIORef counterRef

          -- for some reason warnings only appear if they are added on
          -- particular iterations.
          when (counter == 2) addWarningsToContext

          Ghc.tcPluginIO $ modifyIORef' counterRef succ

          pure $ Just (Ghc.EvExpr Ghc.unitExpr, ct)

    go _ = pure Nothing

-- | Add warnings from the global state back into the GHC context
addWarningsToContext :: Ghc.TcPluginM ()
addWarningsToContext = do
  errsRef <- Ghc.tcl_errs . snd <$> Ghc.getEnvs

  pruneDeleted
  pinnedWarns <- Ghc.unionManyBags . M.elems
             <$> Ghc.tcPluginIO (readMVar globalState)

  Ghc.tcPluginIO . atomicModifyIORef' errsRef
    $ \(warnings, errors) ->
        ((Ghc.unionBags pinnedWarns warnings, errors), ())

-- | Remove warnings for modules that no longer exist
pruneDeleted :: Ghc.TcPluginM ()
pruneDeleted = Ghc.tcPluginIO . modifyMVar_ globalState $ \warns -> do
  let mods = M.keys warns

  deletedMods <-
    filterM (fmap not . Dir.doesFileExist . BS.unpack)
            mods

  pure $ foldl' (flip M.delete) warns deletedMods

-- | After type checking a module, pin any warnings pertaining to it.
insertModuleWarnings :: Ghc.ModSummary -> Ghc.TcGblEnv -> Ghc.TcM Ghc.TcGblEnv
insertModuleWarnings modSummary tcGblEnv = do
  lclErrsRef <- Ghc.tcl_errs . Ghc.env_lcl <$> Ghc.getEnv
  (warns, _) <- liftIO $ readIORef lclErrsRef

  let modFile = BS.pack $ Ghc.ms_hspp_file modSummary
      onlyThisMod w =
        case Ghc.errMsgSpan w of
          Ghc.RealSrcSpan' span ->
            Ghc.bytesFS' (Ghc.srcSpanFile span) == modFile
          _ -> False

      warnsForMod = Ghc.filterBag onlyThisMod warns

  -- Replace any existing pinned warnings with new ones for this module
  liftIO . modifyMVar_ globalState
    $ pure . M.insert modFile warnsForMod

  pure tcGblEnv

