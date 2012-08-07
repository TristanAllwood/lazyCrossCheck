{-# LANGUAGE GADTs #-}
module LazyCrossCheck.Primitives where

import Data.Typeable

data Proxy a = Proxy

ints :: Proxy Int
ints = undefined

data Primitives where
  Primitives :: Typeable a => [a] -> Primitives

(==>) :: Typeable a => Proxy a -> [a] -> Primitives
_ ==> xs = Primitives xs
