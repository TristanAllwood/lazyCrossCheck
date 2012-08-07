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

data LCCSpec n where
  LCCSpec :: CrossCheck (AddResult a n) => a -> a -> [ Primitives ] -> LCCSpec n

lcc :: LCCSpec n -> IO ()
lcc (LCCSpec l r ps) = do
  putStrLn "lcc"

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

instance (Data a, Typeable a, Show a) => Argument a where
  initial primitives = Arg { value  = throw (Path [])
                           , rep    = "?"
                           , refine = urk primitives undefined
                           }



urk :: (Data a, Typeable a, Show a) => [Primitives] -> a -> Path -> [Arg a]
urk ps _ (Path [])
  | Just vs <- findPrimitives ps (undefined :: Proxy a)
  = let mkArg a = Arg { value = a
                      , rep = show a
                      , refine = error "Cannot refine a primitive"
                      }
    in  map mkArg vs
  | otherwise = error "TODO: urk"
