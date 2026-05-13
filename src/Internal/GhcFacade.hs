{-# LANGUAGE CPP #-}
module Internal.GhcFacade
  ( module X
  ) where

import GHC as X hiding (FunDep)
import GHC.Core.Class as X
import GHC.Core.DataCon as X
import GHC.Core.Make as X
import GHC.Data.Bag as X
import GHC.Data.FastString as X
import GHC.Data.IOEnv as X
import GHC.Driver.Plugins as X hiding (TcPlugin)
import GHC.Driver.Env.Types as X
import GHC.Tc.Errors.Types as X
#if MIN_VERSION_ghc(9,14,0)
  hiding (HoleError)
#endif
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
