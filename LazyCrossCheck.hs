{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module LazyCrossCheck
( module LazyCrossCheck.Result
, module LazyCrossCheck.Primitives
, (-->)
, lazyCrossCheck
, lazyCrossCheck2
, with
) where

import Control.Monad.State
import Data.Data
import Data.List
import Prelude hiding (catch)
import Text.PrettyPrint as PP

import LazyCrossCheck.Result
import LazyCrossCheck.Primitives
import LazyCrossCheck.Types
import LazyCrossCheck.Expressions
import LazyCrossCheck.ComparisonTable
import LazyCrossCheck.Utils

lcc :: Int -> String -> LCCSpec n -> IO ()
lcc depth name (LCCSpec exp act ps) = do
  let expectedExp = initialExp exp
  expOuts <- explore depth ps expectedExp
  let simpleExpOuts = map (\(e,r) -> (simplifyExpression name e, r)) expOuts

  let actualExp = initialExp act
  actOuts <- explore depth ps actualExp
  let simpleActOuts = map (\(e,r) -> (simplifyExpression name e, r)) actOuts

  let ct = buildComparisonTable simpleExpOuts simpleActOuts
  printCT ct

printCT :: ComparisonTable -> IO ()
printCT ct@(ComparisonTable rows) = do
  let (passes, possible) = tableStats ct
  putStrLn $ "Results Summary: " ++ show passes ++ " / " ++ show possible

  forM_ rows $ \row -> do
    let txt = PP.renderStyle (PP.style { lineLength = 72 }) (format row)
    putStrLn txt

lazyCrossCheck :: Int -> String -> LCCSpec One -> IO ()
lazyCrossCheck = lcc

lazyCrossCheck2 :: Int -> String -> LCCSpec Two -> IO ()
lazyCrossCheck2 = lcc

(-->) :: forall a n . (CrossCheck (AddResult a n), Resultify a n) =>
         a -> a -> LCCSpec n
l --> r = LCCSpec (addResult l (undefined :: n))
                  (addResult r (undefined :: n)) []

with :: LCCSpec n -> [ Primitives ] -> LCCSpec n
with (LCCSpec l r ps) ps' = LCCSpec l r (ps ++ ps')


instance (Typeable a, Data a, CrossCheck b) => CrossCheck (a -> b) where
  type Res (a -> b) = Res b
  argDatas (_ :: a -> b) = DataExists (undefined :: Proxy a) :
                            argDatas (undefined :: b)
  argReps (_ :: a -> b) = (typeOf (undefined :: a), dataTypeOf (undefined :: a)) : argReps (undefined :: b)
  apply f (a:as) = (f `applyArg` a) `apply` as
  apply _ [] = error "Impossible!"

instance (Eq a, Typeable a, Show a) => CrossCheck (Result a) where
  type Res (Result a) = a

  argReps _ = []
  argDatas _ = []

  apply (Result x) [] = x
  apply (Result _) (_ : _) = error "Impossible!"

