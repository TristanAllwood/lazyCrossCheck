{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module LazyCrossCheck.Types where

import Control.Exception
import Data.Data
import Data.List
import Prelude hiding (catch)
import Text.PrettyPrint as PP

import LazyCrossCheck.Utils
import LazyCrossCheck.Primitives
import LazyCrossCheck.NEList


class (Eq (Res a), Show (Res a), Typeable (Res a)) => CrossCheck a where
  type Res a
  argReps     :: a -> [(TypeRep, DataType)]
  apply       :: a -> [Arg] -> Res a
  argDatas    :: a -> [DataExists]

data LCCSpec n where
  LCCSpec :: CrossCheck a => a -> a -> [ Primitives ] -> LCCSpec n

data LTGSpec n where
  LTGSpec :: CrossCheck a => a -> [ Primitives ] -> LTGSpec n

class Format a where
  format :: a -> Doc

{- module ComparisonTable -}

data ComparisonTable
  = ComparisonTable [ComparisonRow]
  deriving Show

data ComparisonRow
  = IdenticalExp SimpleExp EvalResult EvalResult
  | ExpectedMoreGeneral SimpleExp EvalResult [(SimpleExp, EvalResult)]
  | ActualMoreGeneral [(SimpleExp, EvalResult)] SimpleExp EvalResult
  | ExpectedIsolated SimpleExp EvalResult
  | ActualIsolated SimpleExp EvalResult
  deriving Show

data Related = Identical | MoreGeneral | LessGeneral | Unrelated
  deriving (Show, Eq)


{- module LazyCrossCheck.Expressions -}

data Path = Path (NEList Int)
  deriving (Eq, Read, Show, Typeable)

instance Exception Path where

data Exp
  = forall a . CrossCheck a =>
    Exp { root      :: CrossCheck a => a
        , arguments :: [Arg]
        }

data Arg
  = forall a . (Typeable a, Data a) => ArgConstr (Proxy a) Constr [Arg]
  | forall a . (Typeable a, Data a) => ArgUndefined (Proxy a) Path
  | forall a . (Eq a, Show a, Typeable a) => ArgPrimitive a

instance Show Exp where
  show (Exp { arguments }) = show arguments

instance Show Arg where
  show (ArgConstr _ c as)   = unwords [show c, show as]
  show (ArgUndefined _ p) = unwords ["?", show p]
  show (ArgPrimitive p)   = show p


data SimpleExp = SimpleExp String [SimpleArg]
  deriving (Eq, Read, Show)

data SimpleArg
  = SimpleConstr String [SimpleArg]
  | SimpleUndefined Path
  | SimplePrimitive String
  deriving (Eq, Read, Show)

instance Format SimpleExp where
  format (SimpleExp name args) = hsep (text name : map format args)

instance Format SimpleArg where
  format (SimpleConstr c as)
    | length as == 0  = text c
    | otherwise       = parens $ (hsep $ (text c):map format as)
  format (SimpleUndefined _) = text "undefined"
  format (SimplePrimitive p)   = text p

{- module LazyCrossCheck.Eval -}

data EvalResult
  = EvalSuccess String
  | EvalUndefined Path
  | EvalException String
  deriving (Read, Show)

