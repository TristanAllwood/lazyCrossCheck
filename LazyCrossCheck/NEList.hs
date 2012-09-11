{-# LANGUAGE DeriveDataTypeable #-}
module LazyCrossCheck.NEList where

import Data.Typeable

data NEList a
  = NENil a
  | NECons a (NEList a)
  deriving (Eq, Ord, Show, Read, Typeable)

neSnoc :: NEList a -> a -> NEList a
neSnoc xs a = go xs
  where
    go (NECons p ps) = NECons p (go ps)
    go (NENil p) = NECons p (NENil a)

