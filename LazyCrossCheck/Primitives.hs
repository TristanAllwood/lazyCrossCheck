{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module LazyCrossCheck.Primitives where

import Data.Maybe
import Data.Typeable

data Proxy a = Proxy

ints :: Proxy Int
ints = undefined

data Primitives where
  Primitives :: (Show a, Typeable a) => [a] -> Primitives

(==>) :: (Show a, Typeable a) => Proxy a -> [a] -> Primitives
_ ==> xs = Primitives xs

findPrimitives :: [Primitives] -> TypeRep
               -> (forall a . (Show a, Typeable a) => a -> b) -> [b]
findPrimitives ps rep f = concatMap extract ps
  where
    extract (Primitives (xs :: [b]))
      | typeOf (undefined :: b) == rep = map f xs
      | otherwise = []
