{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ShowWarnings
  ( ShowWarnings
  , showWarnings
  ) where

class ShowWarnings where
  showWarnings :: ()
