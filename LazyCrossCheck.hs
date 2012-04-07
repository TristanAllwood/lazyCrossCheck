{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module LazyCrossCheck where

import Data.Typeable
import Data.Data

lcc :: LCCSpec n -> IO ()
lcc _ = error "TODO: lcc"

lazyCrossCheck :: LCCSpec One -> IO ()
lazyCrossCheck = lcc

lazyCrossCheck2 :: LCCSpec Two -> IO ()
lazyCrossCheck2 = lcc

data Primitives where
  Primitives :: Typeable a => [a] -> Primitives

data Proxy a = Proxy

ints :: Proxy Int
ints = undefined

(-->) :: CrossCheck (AddResult a n) => a -> a -> LCCSpec n
(-->) = error $ "TODO: -->"

(==>) :: Typeable a => Proxy a -> [a] -> Primitives
_ ==> xs = Primitives xs

with :: LCCSpec n -> [ Primitives ] -> LCCSpec n
with = error $ "TODO: with"

data Zero
data Succ n

type One = Succ Zero
type Two = Succ One
type Three = Succ Two

data LCCSpec n = LCCSpec

data Result a = Result a

type family  AddResult a n
type instance AddResult a Zero = Result a
type instance AddResult (a -> b) (Succ n) = a -> (AddResult b n)

class Argument a where
class CrossCheck a where

instance (Typeable a, Show a) => Argument a

instance (Argument a, CrossCheck b) => CrossCheck (a -> b)

instance (Eq a, Show a) => CrossCheck (Result a)
