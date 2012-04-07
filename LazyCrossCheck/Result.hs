{-# LANGUAGE TypeFamilies #-}
module LazyCrossCheck.Result where

data Zero
data Succ n

type One = Succ Zero
type Two = Succ One
type Three = Succ Two

data Result a = Result a

type family  AddResult a n
type instance AddResult a Zero = Result a
type instance AddResult (a -> b) (Succ n) = a -> (AddResult b n)
