{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module Internal.GhcFacade
  ( module X
  , TcPluginResult'
  , pattern CDictCan'
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
import GHC.Driver.Config.Diagnostic as X

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
import GHC.Driver.Config.Diagnostic as X

#endif

#if MIN_VERSION_ghc(9,4,0)
type TcPluginResult' = X.TcPluginSolveResult
#else
type TcPluginResult' = X.TcPluginResult
#endif

pattern CDictCan'
  :: CtEvidence
  -> Class
  -> [Xi]
  -> Ct
pattern CDictCan' diEv diCls diTys
#if MIN_VERSION_ghc(9,8,0)
  <- CDictCan (DictCt diEv diCls diTys _)
#else
  <- CDictCan { cc_ev = diEv, cc_class = diCls, cc_tyargs = diTys }
#endif
