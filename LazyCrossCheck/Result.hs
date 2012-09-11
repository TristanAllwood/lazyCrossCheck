{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module LazyCrossCheck.Result where

data Zero
data Succ n

type One = Succ Zero
type Two = Succ One
type Three = Succ Two

newtype Result a = Result a

data Nat a n where
  NZ :: Nat a Zero
  NS :: Nat b n -> Nat (a -> b) (Succ n)

type family  AddResult a n
type instance AddResult a Zero = Result a
type instance AddResult (a -> b) (Succ n) = a -> (AddResult b n)

class Resultify a n where
  addResult :: a -> n -> AddResult a n

instance Resultify a Zero where
  addResult a _ = Result a

instance Resultify b n => Resultify (a -> b) (Succ n) where
  addResult f (_ :: Succ n) = \x -> addResult (f x) (undefined :: n)
