{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module Internal.GhcFacade
  ( module X
  , pattern RealSrcLoc'
#if MIN_VERSION_ghc(9,2,0)
#else
  , log_action'
#endif
  , TcPluginResult'
  ) where

#if MIN_VERSION_ghc(9,6,0)
import GHC as X hiding (FunDep)
import GHC.Core.Class as X
import GHC.Core.Make as X
import GHC.Data.Bag as X
import GHC.Data.FastString as X
import GHC.Data.IOEnv as X
import GHC.Driver.Plugins as X hiding (TcPlugin)
import GHC.Driver.Env.Types as X
import GHC.Tc.Errors.Types as X
import GHC.Tc.Plugin as X
import GHC.Tc.Types as X hiding (DefaultingPlugin)
import GHC.Tc.Types.Constraint as X
import GHC.Tc.Types.Evidence as X
import GHC.Types.Error as X
import GHC.Types.Name.Occurrence as X
import GHC.Types.SrcLoc as X
import GHC.Utils.Error as X
import GHC.Utils.Logger as X
import GHC.Utils.Outputable as X

#elif MIN_VERSION_ghc(9,4,0)
import GHC as X hiding (FunDep)
import GHC.Core.Class as X
import GHC.Core.Make as X
import GHC.Data.Bag as X
import GHC.Data.FastString as X
import GHC.Data.IOEnv as X
import GHC.Driver.Plugins as X hiding (TcPlugin)
import GHC.Driver.Env.Types as X
import GHC.Tc.Errors.Types as X
import GHC.Tc.Plugin as X
import GHC.Tc.Types as X hiding (DefaultingPlugin)
import GHC.Tc.Types.Constraint as X
import GHC.Tc.Types.Evidence as X
import GHC.Types.Error as X
import GHC.Types.Name.Occurrence as X
import GHC.Types.SrcLoc as X
import GHC.Utils.Error as X
import GHC.Utils.Logger as X
import GHC.Utils.Outputable as X

#elif MIN_VERSION_ghc(9,2,0)
import GHC as X hiding (FunDep)
import GHC.Core.Class as X
import GHC.Core.Make as X
import GHC.Data.Bag as X
import GHC.Data.FastString as X
import GHC.Data.IOEnv as X
import GHC.Driver.Plugins as X hiding (TcPlugin)
import GHC.Driver.Env.Types as X
import GHC.Tc.Plugin as X
import GHC.Tc.Types as X
import GHC.Tc.Types.Constraint as X
import GHC.Tc.Types.Evidence as X
import GHC.Types.Error as X
import GHC.Types.Name.Occurrence as X
import GHC.Types.SrcLoc as X
import GHC.Utils.Error as X
import GHC.Utils.Logger as X
import GHC.Utils.Outputable as X

#elif MIN_VERSION_ghc(9,0,0)
import GHC as X
import GHC.Core.Class as X
import GHC.Core.Make as X
import GHC.Data.Bag as X
import GHC.Data.FastString as X
import GHC.Data.IOEnv as X
import GHC.Driver.Plugins as X hiding (TcPlugin)
import GHC.Driver.Session
import GHC.Driver.Types as X
import GHC.Tc.Plugin as X
import GHC.Tc.Types as X
import GHC.Tc.Types.Constraint as X
import GHC.Tc.Types.Evidence as X
import GHC.Types.Name.Occurrence as X
import GHC.Types.SrcLoc as X
import GHC.Utils.Error as X

#elif MIN_VERSION_ghc(8,10,0)
import Bag as X
import Class as X
import Constraint as X
import DynFlags as X
import ErrUtils as X
import FastString as X
import GHC as X
import HscMain as X
import HscTypes as X
import IOEnv as X
import MkCore as X
import OccName as X
import Outputable as X
import Plugins as X hiding (TcPlugin)
import SrcLoc as X
import TcEvidence as X
import TcPluginM as X
import TcRnTypes as X
#endif

pattern RealSrcLoc' :: RealSrcLoc -> SrcLoc
pattern RealSrcLoc' s <-
#if MIN_VERSION_ghc(9,0,1)
    RealSrcLoc s _
#elif MIN_VERSION_ghc(8,10,0)
    RealSrcLoc s
#endif

#if MIN_VERSION_ghc(9,2,0)
#else
log_action' :: LogAction -> (DynFlags -> Severity -> SrcSpan -> MsgDoc -> IO ()) -> LogAction
#endif
#if MIN_VERSION_ghc(9,2,0)
#elif MIN_VERSION_ghc(9,0,1)
log_action' action withStuff dflags warnReason severity srcSpan msgDoc = do
  withStuff dflags severity srcSpan msgDoc
  action dflags warnReason severity srcSpan msgDoc
#elif MIN_VERSION_ghc(8,10,0)
log_action' action withStuff dflags warnReason severity srcSpan pprStyle msgDoc = do
  withStuff dflags severity srcSpan msgDoc
  action dflags warnReason severity srcSpan pprStyle msgDoc
#endif

#if MIN_VERSION_ghc(9,4,0)
type TcPluginResult' = X.TcPluginSolveResult
#else
type TcPluginResult' = X.TcPluginResult
#endif
