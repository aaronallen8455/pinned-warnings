{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
module Internal.Types
  ( ModuleFile
  , Warning(..)
  , showWarning
  , MonoidMap(..)
  , SrcSpanKey
  , WarningsWithModDate(..)
  ) where

import qualified Data.Map.Strict as M
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Data.Time

import qualified Internal.GhcFacade as Ghc

type ModuleFile = String

newtype Warning = Warning
  { unWarning
#if MIN_VERSION_ghc(9,4,0)
      :: Ghc.MsgEnvelope Ghc.DiagnosticMessage
#elif MIN_VERSION_ghc(9,2,0)
      :: Ghc.MsgEnvelope Ghc.DecoratedSDoc
#else
      :: Ghc.WarnMsg
#endif
  }

showWarning :: Warning -> String
showWarning =
#if MIN_VERSION_ghc(9,4,0)
  let sdocCtx = Ghc.defaultSDocContext
              { Ghc.sdocPrintUnicodeSyntax = True
              , Ghc.sdocCanUseUnicode = True
              }
   in foldMap (Ghc.showSDocOneLine sdocCtx)
      . Ghc.unDecorated
#if MIN_VERSION_ghc(9,6,0)
      . Ghc.diagnosticMessage Ghc.NoDiagnosticOpts
#else
      . Ghc.diagnosticMessage
#endif
      . Ghc.errMsgDiagnostic . unWarning
#elif MIN_VERSION_ghc(9,2,0)
  let sdocCtx = Ghc.defaultSDocContext
              { Ghc.sdocPrintUnicodeSyntax = True
              , Ghc.sdocCanUseUnicode = True
              }
   in foldMap (Ghc.showSDocOneLine sdocCtx)
      . Ghc.unDecorated . Ghc.errMsgDiagnostic . unWarning
#else
  show . unWarning
#endif

instance Eq Warning where
  Warning a == Warning b = show a == show b

instance Ord Warning where
  compare = comparing (show . unWarning)

newtype MonoidMap k a = MonoidMap (M.Map k a)
  deriving Foldable

instance (Ord k, Semigroup a) => Semigroup (MonoidMap k a) where
  MonoidMap a <> MonoidMap b = MonoidMap $ M.unionWith (<>) a b

instance (Ord k, Semigroup a) => Monoid (MonoidMap k a) where
  mempty = MonoidMap M.empty

type SrcSpanKey = (Ghc.RealSrcLoc, Ghc.RealSrcLoc) -- start and end of span

data WarningsWithModDate =
  MkWarningsWithModDate
    { lastUpdated :: !UTCTime -- Last time the module was modified
    , warningsMap :: !(MonoidMap SrcSpanKey (S.Set Warning))
    }

instance Semigroup WarningsWithModDate where
  a <> b = MkWarningsWithModDate
             (max (lastUpdated a) (lastUpdated b))
             (warningsMap a <> warningsMap b)
