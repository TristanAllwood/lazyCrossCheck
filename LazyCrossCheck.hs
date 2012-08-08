{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Data.Data
import Data.List
import Data.Maybe
import Data.Typeable
import Prelude hiding (catch)
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Process

import LazyCrossCheck.Result
import LazyCrossCheck.Primitives

data LCCSpec n where
  LCCSpec :: CrossCheck a => a -> a -> [ Primitives ] -> LCCSpec n

defaultDepth = 10

lcc :: LCCSpec n -> IO ()
lcc (LCCSpec act exp ps) = do
  let expectedExp = initialExp exp
  outputs <- explore defaultDepth ps expectedExp
  forM_ outputs $ \(exp, res) -> do
    putStrLn $ ("expected ") ++ (show $ arguments exp) ++ " ==> " ++ (show res)

explore :: Int -> [Primitives] -> Exp -> IO [(Exp, EvalResult)]
explore depth primitives exp
  | depth <= 0 = return []
  | otherwise  = do
  res <- eval exp

  case res of
    EvalUndefined path -> do
      let nexts = refine primitives exp path
      concat <$> mapM (explore (depth - 1) primitives) nexts
    _ -> return [(exp, res)]

lazyCrossCheck :: LCCSpec One -> IO ()
lazyCrossCheck = lcc

lazyCrossCheck2 :: LCCSpec Two -> IO ()
lazyCrossCheck2 = lcc

(-->) :: forall a n . (CrossCheck (AddResult a n), Resultify a n) =>
         a -> a -> LCCSpec n
l --> r = LCCSpec (addResult l (undefined :: n)) (addResult r (undefined :: n)) []

with :: LCCSpec n -> [ Primitives ] -> LCCSpec n
with (LCCSpec l r ps) ps' = LCCSpec l r (ps ++ ps')

newtype Path = Path [Int]
  deriving (Eq, Read, Show, Typeable)

instance Exception Path where

data Exp
  = forall a . CrossCheck a =>
    Exp { root      :: CrossCheck a => a
        , arguments :: [Arg]
        }

data Arg
  = ArgConstr Constr [Arg]
  | ArgUndefined Path
  | forall a . (Show a, Typeable a) => ArgPrimitive a

deriving instance Show Arg

getSafeResult :: Show a => a -> IO EvalResult
getSafeResult x = (do
  let str = show x
  () <- evaluate $ foldl' (flip seq) () str
  return $ EvalSuccess str
  ) `catches` [ Handler $ \(p :: Path) -> return $ EvalUndefined p
              , Handler $ \(e :: SomeException) -> return $ EvalException (show e) {- TODO safety of show e-}
              ]

data EvalResult
  = EvalSuccess String
  | EvalUndefined Path
  | EvalException String
  deriving (Read, Show)

(=~=) :: EvalResult -> EvalResult -> Bool
(EvalSuccess s1)   =~= (EvalSuccess s2)   = s1 == s2
(EvalUndefined p1) =~= (EvalUndefined p2) = p1 == p2
(EvalException _)  =~= (EvalException _)  = True
_ =~= _ = False

eval :: Exp -> IO EvalResult
eval exp = do
  (parent, child) <- createPipe
  child_pid <- forkProcess $ do
    child_h <- fdToHandle child
    safeRes <- case exp of
                Exp root arguments -> getSafeResult (apply root arguments)
    hPrint child_h safeRes
    hFlush child_h

  child_h <- fdToHandle child
  hClose child_h

  var <- newEmptyMVar

  {- TODO: add timeouts -}

  forkIO $ (do
    parent_h <- fdToHandle parent
    putMVar var =<< read <$> hGetContents parent_h
    ) `catch` (\(e :: SomeException) -> putMVar var $ EvalException (show e))

  mRes <- getProcessStatus True False child_pid
  res <- takeMVar var
  return res


refine :: [Primitives] -> Exp -> Path -> [Exp]
refine = error "TODO: refine"

evalArg :: (Data a, Typeable a) => Arg -> a
evalArg (ArgConstr c as)    = flip evalState as $ do
  fromConstrM (evalArg <$> next) c
  where
    next = state (\(x:xs) -> (x, xs))
evalArg (ArgUndefined path) = throw path
evalArg (ArgPrimitive prim) = fromMaybe (error "evalArg Prim cast") $ cast prim

applyArg :: (Typeable a, Data a) => (a -> b) -> Arg -> b
applyArg f arg = let a = evalArg arg in f a

initialExp :: CrossCheck a => a -> Exp
initialExp f = Exp { root = f
                   , arguments = [ ArgUndefined (Path [i]) |
                                    i <- [0 .. argCount f - 1]]
                   }

class (Eq (Res a), Show (Res a), Typeable (Res a)) => CrossCheck a where
  type Res a
  argCount :: a -> Int

  apply :: a -> [Arg] -> Res a

instance (Typeable a, Data a, CrossCheck b) => CrossCheck (a -> b) where
  type Res (a -> b) = Res b
  argCount (_ :: a -> b) = 1 + argCount (undefined :: b)
  apply f (a:as) = (f `applyArg` a) `apply` as

instance (Eq a, Typeable a, Show a) => CrossCheck (Result a) where
  type Res (Result a) = a

  argCount _ = 0

  apply (Result x) [] = x

