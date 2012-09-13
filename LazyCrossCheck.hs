{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module LazyCrossCheck
( module LazyCrossCheck.Result
, module LazyCrossCheck.Primitives
, (-->)
, testGen
, lazyCrossCheck
, lazyCrossCheck2
, lazyTestGeneration
, lazyTestGeneration2
, crossCheckFiles
, with
) where

import Control.Applicative
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

crossCheckFiles :: FilePath -> FilePath -> IO ()
crossCheckFiles expected actual = do
  expectedOuts <- read <$> readFile expected
  actualOuts   <- read <$> readFile actual
  let ct = buildComparisonTable expectedOuts actualOuts
  printCT ct

ltg :: Int -> String -> LTGSpec n -> IO [(SimpleExp, EvalResult)]
ltg depth name (LTGSpec exp ps) = do
  outs <- explore depth ps (initialExp exp)
  return $ map (\(e,r) -> (simplifyExpression name e, r)) outs

lcc :: Int -> String -> LCCSpec n -> IO ()
lcc depth name (LCCSpec exp act ps) = do
  simpleExpOuts <- ltg depth name (LTGSpec exp ps)
  simpleActOuts <- ltg depth name (LTGSpec act ps)

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

lazyTestGeneration :: Int -> String -> LTGSpec One
                   -> IO [(SimpleExp, EvalResult)]
lazyTestGeneration = ltg

lazyTestGeneration2 :: Int -> String -> LTGSpec Two
                    -> IO [(SimpleExp, EvalResult)]
lazyTestGeneration2 = ltg

(-->) :: forall a n . (CrossCheck (AddResult a n), Resultify a n) =>
         a -> a -> LCCSpec n
l --> r = LCCSpec (addResult l (undefined :: n))
                  (addResult r (undefined :: n)) []

testGen :: forall a n . (CrossCheck (AddResult a n), Resultify a n) =>
           a -> LTGSpec n
testGen e = LTGSpec (addResult e (undefined :: n)) []

class With a where
  with :: a n -> [ Primitives ] -> a n

instance With LCCSpec where
  with (LCCSpec l r ps) ps' = LCCSpec l r (ps ++ ps')

instance With LTGSpec where
  with (LTGSpec e ps) ps' = LTGSpec e (ps ++ ps')

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

