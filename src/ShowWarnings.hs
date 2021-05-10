{-# LANGUAGE MultiParamTypeClasses #-}
module ShowWarnings
  ( ShowWarnings
  , showWarnings
  , FixWarnings
  , fixWarnings
  ) where

class ShowWarnings where
  -- | Display currently pinned warnings
  showWarnings :: ()

class FixWarnings where
  -- | Auto-fixes certain types of warnings
  fixWarnings :: ()
