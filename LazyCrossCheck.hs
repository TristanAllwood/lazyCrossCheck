{-# LANGUAGE DeriveDataTypeable #-}
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

import Control.Exception
import Data.Data
import Data.Typeable

import LazyCrossCheck.Result
import LazyCrossCheck.Primitives

lcc :: LCCSpec n -> IO ()
lcc _ = error "TODO: lcc"

lazyCrossCheck :: LCCSpec One -> IO ()
lazyCrossCheck = lcc

lazyCrossCheck2 :: LCCSpec Two -> IO ()
lazyCrossCheck2 = lcc

(-->) :: CrossCheck (AddResult a n) => a -> a -> LCCSpec n
l --> r = LCCSpec l r []

with :: LCCSpec n -> [ Primitives ] -> LCCSpec n
with (LCCSpec l r ps) ps' = LCCSpec l r (ps ++ ps')

data Path = Path [Int]
  deriving (Show, Typeable)

instance Exception Path where

data LCCSpec n where
  LCCSpec :: CrossCheck (AddResult a n) => a -> a -> [ Primitives ] -> LCCSpec n

data Arg a
  = Arg { value   :: a
        , rep     :: String
        , refine  :: Path -> [Arg a]
        }

class Argument a where
  initial :: [Primitives] -> Arg a

class CrossCheck a where

instance (Argument a, CrossCheck b) => CrossCheck (a -> b)
instance (Eq a, Show a) => CrossCheck (Result a)

instance (Argument a) => Argument (Maybe a) where
  initial = argumentData

instance Argument Int where
  initial = usePrimitive

argumentData ::
