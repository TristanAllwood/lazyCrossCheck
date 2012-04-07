{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module LazyCrossCheck
( module LazyCrossCheck.Result
, module LazyCrossCheck.Primitives
, (-->)
, lazyCrossCheck
, with
) where

import Data.Typeable
import Data.Data

import LazyCrossCheck.Result
import LazyCrossCheck.Primitives

lcc :: LCCSpec n -> IO ()
lcc _ = error "TODO: lcc"

lazyCrossCheck :: LCCSpec One -> IO ()
lazyCrossCheck = lcc

lazyCrossCheck2 :: LCCSpec Two -> IO ()
lazyCrossCheck2 = lcc

(-->) :: CrossCheck (AddResult a n) => a -> a -> LCCSpec n
(-->) = error $ "TODO: -->"

with :: LCCSpec n -> [ Primitives ] -> LCCSpec n
with = error $ "TODO: with"

data LCCSpec n = LCCSpec

class Argument a where
class CrossCheck a where

instance (Typeable a, Show a) => Argument a

instance (Argument a, CrossCheck b) => CrossCheck (a -> b)

instance (Eq a, Show a) => CrossCheck (Result a)
