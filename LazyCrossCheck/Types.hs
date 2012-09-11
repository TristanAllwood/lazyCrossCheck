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

class Format a where
  format :: a -> Doc

{- module ComparisonTable -}

data ComparisonTable
  = ComparisonTable [ComparisonRow]
  deriving Show

data ComparisonRow
  = IdenticalExp Exp EvalResult EvalResult
  | ExpectedMoreGeneral Exp EvalResult [(Exp, EvalResult)]
  | ActualMoreGeneral [(Exp, EvalResult)] Exp EvalResult
  | ExpectedIsolated Exp EvalResult
  | ActualIsolated Exp EvalResult
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

instance Format Exp where
  format (Exp { arguments }) = hsep (map format arguments)


instance Format Arg where
  format (ArgConstr _ c as)
    | length as == 0  = text $ show c
    | otherwise       = parens $ (hsep (text (show c):map format as))
  format (ArgUndefined _ _) = text "undefined"
  format (ArgPrimitive p)   = text $ show p

instance Show Arg where
  show (ArgConstr _ c as)   = unwords [show c, show as]
  show (ArgUndefined _ p) = unwords ["?", show p]
  show (ArgPrimitive p)   = show p

{- module LazyCrossCheck.Eval -}

data EvalResult
  = EvalSuccess String
  | EvalUndefined Path
  | EvalException String
  deriving (Read, Show)
