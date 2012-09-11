{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyCrossCheck.Eval where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.List
import Prelude hiding (catch)
import System.IO
import System.Posix.IO
import System.Posix.Process
import Text.PrettyPrint as PP

import LazyCrossCheck.Types

getSafeResult :: Show a => a -> IO EvalResult
getSafeResult x = (do
  let str = show x
  () <- evaluate $ foldl' (flip seq) () str
  return $ EvalSuccess str
  ) `catches` [ Handler $ \(p :: Path) -> return $ EvalUndefined p
              , Handler $ \(e :: SomeException) -> return $ EvalException (show e)
              ] `catches` [ Handler $ \(p :: Path) -> return $ EvalUndefined p
                          , Handler $ \(_ :: SomeException) -> return $ EvalException "nested exception"
                          ]


instance Format EvalResult where
  format (EvalSuccess str)      = text str
  format (EvalUndefined _)      = "undefined"
  format (EvalException string) = "Exception:" <+> text string


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

  _ <- forkIO $ (do
    parent_h <- fdToHandle parent
    putMVar var =<< read <$> hGetContents parent_h
    ) `catch` (\(e :: SomeException) -> putMVar var $ EvalException (show e))

  _ <- getProcessStatus True False child_pid
  res <- takeMVar var
  return res

failedPrecondition :: EvalResult -> Bool
failedPrecondition (EvalException _) = True
failedPrecondition _                 = False

(~~>) :: EvalResult -> EvalResult -> Bool
(EvalException _) ~~> _                   = True
(EvalUndefined p1) ~~> (EvalUndefined p2) = p1 == p2
(EvalSuccess s1) ~~> (EvalSuccess s2)     = s1 == s2
_ ~~> _ = False
