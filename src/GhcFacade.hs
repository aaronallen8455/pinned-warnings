{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module GhcFacade
  ( module X
  , pattern RealSrcSpan'
  , bytesFS'
  ) where

import qualified Data.ByteString.Char8 as BS

#if MIN_VERSION_ghc(9,0,0)
import GHC.Data.Bag as X
import GHC.Tc.Types as X
import GHC.Tc.Plugin as X
import GHC.Driver.Plugins as X hiding (TcPlugin)
import GHC.Data.IOEnv as X
import GHC.Core.Make as X
import GHC.Tc.Types.Evidence as X
import GHC as X
import GHC.Data.FastString as X
import GHC.Utils.Error as X
import GHC.Core.Class as X
import GHC.Tc.Types.Constraint as X
import GHC.Types.Name.Occurrence as X

#elif MIN_VERSION_ghc(8,8,0)
import Bag as X
import Class as X
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
import TcEvidence as X
import TcPluginM as X
import TcRnTypes as X
#endif

#if MIN_VERSION_ghc(8,10,0) && __GLASGOW_HASKELL__ < 900
import Constraint as X
#endif

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
pattern RealSrcSpan' s <-
#if MIN_VERSION_ghc(9,0,1)
    RealSrcSpan s x
#elif MIN_VERSION_ghc(8,8,0)
    RealSrcSpan s
#endif

bytesFS' :: FastString -> BS.ByteString
bytesFS' =
#if MIN_VERSION_ghc(8,10,0)
  bytesFS
#elif MIN_VERSION_ghc(8,8,0)
  BS.pack . unpackFS
#endif
