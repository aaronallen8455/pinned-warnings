{-# LANGUAGE MultiParamTypeClasses #-}
module ShowWarnings
  ( ShowWarnings
  , showWarnings
  , FixWarnings
  , fixWarnings
  , ClearWarnings
  , clearWarnings
  ) where

class ShowWarnings where
  -- | Display currently pinned warnings
  showWarnings :: ()

class FixWarnings where
  -- | Auto-fixes certain types of warnings
  fixWarnings :: ()

class ClearWarnings where
  -- | Removes any currently pinned warnings
  clearWarnings :: ()
