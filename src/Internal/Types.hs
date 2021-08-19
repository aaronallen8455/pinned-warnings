{-# LANGUAGE DeriveFoldable #-}
module Internal.Types
  ( ModuleFile
  , Warning(..)
  , MonoidMap(..)
  , SrcSpanKey
  , WarningsWithModDate(..)
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Time

import qualified Internal.GhcFacade as Ghc

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
