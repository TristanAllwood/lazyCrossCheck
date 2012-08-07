{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module LazyCrossCheck.Primitives where

import Data.Maybe
import Data.Typeable

data Proxy a = Proxy

ints :: Proxy Int
ints = undefined

data Primitives where
  Primitives :: Typeable a => [a] -> Primitives

(==>) :: Typeable a => Proxy a -> [a] -> Primitives
_ ==> xs = Primitives xs

findPrimitives :: forall a . Typeable a => [Primitives] -> Proxy a -> Maybe [a]
findPrimitives ps _
  | null possibles = Nothing
  | otherwise      = Just (concat possibles)
  where
    possibles = mapMaybe extract ps

    extract (Primitives (xs :: [b]))
      | typeOf (undefined :: b) == typeOf (undefined :: a)
      = Just $ mapMaybe cast xs
      | otherwise = Nothing
