{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Text.PrettyPrint as PP

import NEList
import LazyCrossCheck.Result
import LazyCrossCheck.Primitives

data LCCSpec n where
  LCCSpec :: CrossCheck a => a -> a -> [ Primitives ] -> LCCSpec n

defaultDepth = 5

lcc :: String -> LCCSpec n -> IO ()
lcc name (LCCSpec exp act ps) = do
  let expectedExp = initialExp exp
  expOuts <- explore defaultDepth ps expectedExp

  let actualExp = initialExp act
  actOuts <- explore defaultDepth ps actualExp

  let (ComparisonTable rows) = buildComparisonTable expOuts actOuts

  forM_ rows (formatRow name)


formatRow :: String -> ComparisonRow -> Doc
formatRow name (IdenticalExp e l r)
  | l ~~> r   = PP.empty
  | otherwise = vcat [ text (name <+> format e <> ": results differ")
                     , nest 2 $ vcat [ text $ "model answer:" <+> format l
                                     , text $ "student:"      <+> format r
                                     ]
                     ]
formatRow name (ExpectedMoreGeneral e l rs)
  | failedPrecondition l = PP.empty
  | otherwise
  = vcat [ text $ name <+> format e <> ": model answer is more general"
         , nest 2 $ vcat $ [ text $ "model answer:" <+> format l
                           , text $ "student answers:"
                           , nest 1 kids
                           ]
       ]
  where
    kids = vcat (map formatRow' rs)
    formatRow' (e', r) = text $ name <+> format e' ":" <+> format r

formatRow name (ActualMoreGeneral ls e rs)
  |

class Format a where
  format :: a -> String

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

failedPrecondition :: EvalResult -> Bool
failedPrecondition (EvalException _) = True
failedPrecondition _                 = False

(~~>) :: EvalResult -> EvalResult -> Bool
(EvalException _) ~~> _                   = True
(EvalUndefined p1) ~~> (EvalUndefined p2) = p1 == p2
(EvalSuccess s1) ~~> (EvalSuccess s2)     = s1 == s2

buildComparisonTable :: [(Exp, EvalResult)] -> [(Exp, EvalResult)]
                     -> ComparisonTable
buildComparisonTable expected actual
  = ComparisonTable . concat $ unfoldr (uncurry oneStep) (expected, actual)
  where
    oneStep :: [(Exp, EvalResult)] -> [(Exp, EvalResult)] ->
               Maybe ([ComparisonRow], ([(Exp, EvalResult)],[(Exp, EvalResult)]))
    oneStep [] []         = Nothing
    oneStep [] actuals    = let rows = map (uncurry ActualIsolated) actuals
                             in Just (rows, ([], []))
    oneStep expecteds []  = let rows = map (uncurry ExpectedIsolated) expecteds
                             in Just (rows, ([], []))
    oneStep allExpecteds@((expectedExp, expectedResult): expecteds) actuals
      | [(_, actualResult)] <- identicals
      = Just ([IdenticalExp expectedExp expectedResult actualResult],
              (expecteds, unrelateds))

      | (not . null) moreGenerals
      = Just ([ExpectedMoreGeneral expectedExp expectedResult moreGenerals],
              (expecteds, unrelateds))

      | [(mgActualExp, actualResult)] <- lessGenerals
      = let (lessGeneralExpecteds, unrelatedExpecteds)
              = partition ((==) MoreGeneral . expRelated mgActualExp . fst)
                          allExpecteds
         in Just ([ActualMoreGeneral lessGeneralExpecteds mgActualExp actualResult],
                  (unrelatedExpecteds, unrelateds)
                 )

      | otherwise = Just ( [ExpectedIsolated expectedExp expectedResult],
                           (expecteds, actuals))
      where
        actualsRelatedness = map (expRelated expectedExp . fst) actuals

        (concat -> identicals, concat -> moreGenerals,
         concat -> lessGenerals, concat -> unrelateds)
          = unzip4 $ zipWith partitionRelatedness actualsRelatedness actuals

partitionRelatedness :: Related -> a -> ([a],[a],[a],[a])
partitionRelatedness Identical    a = ([a], [], [], [])
partitionRelatedness MoreGeneral  a = ([], [a], [], [])
partitionRelatedness LessGeneral  a = ([], [], [a], [])
partitionRelatedness Unrelated    a = ([], [], [], [a])

expRelated :: Exp -> Exp -> Related
expRelated l r = argsRelated (arguments l) (arguments r)
  where
    argsRelated :: [Arg] -> [Arg] -> Related
    argsRelated ls rs = foldl combineRelated Identical $
                          zipWith argRelated ls rs

    combineRelated :: Related -> Related -> Related
    combineRelated Identical x = x
    combineRelated x Identical = x
    combineRelated MoreGeneral MoreGeneral = MoreGeneral
    combineRelated LessGeneral LessGeneral = LessGeneral
    combineRelated _ _ = Unrelated

    argRelated :: Arg -> Arg -> Related
    argRelated (ArgConstr _ cl ls) (ArgConstr _ cr rs)
      | cl == cr  = argsRelated ls rs
    argRelated (ArgConstr _ _ _) (ArgUndefined _ _)    = LessGeneral
    argRelated (ArgUndefined _ _) (ArgConstr _ _ _)    = MoreGeneral
    argRelated (ArgUndefined _ _) (ArgUndefined _ _) = Identical
    argRelated (ArgUndefined _ _) (ArgPrimitive _)   = MoreGeneral
    argRelated (ArgPrimitive l)   (ArgPrimitive r)
      | Just l' <- cast l
      , l' == r                                      = Identical
    argRelated (ArgPrimitive _)   (ArgUndefined _ _) = LessGeneral
    argRelated _ _ = Unrelated

data Related = Identical | MoreGeneral | LessGeneral | Unrelated
  deriving (Show, Eq)

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

lazyCrossCheck :: String -> LCCSpec One -> IO ()
lazyCrossCheck = lcc

lazyCrossCheck2 :: String -> LCCSpec Two -> IO ()
lazyCrossCheck2 = lcc

(-->) :: forall a n . (CrossCheck (AddResult a n), Resultify a n) =>
         a -> a -> LCCSpec n
l --> r = LCCSpec (addResult l (undefined :: n))
                  (addResult r (undefined :: n)) []

with :: LCCSpec n -> [ Primitives ] -> LCCSpec n
with (LCCSpec l r ps) ps' = LCCSpec l r (ps ++ ps')

data Path = Path (NEList Int)
  deriving (Eq, Read, Show, Typeable)

instance Exception Path where

data Exp
  = forall a . CrossCheck a =>
    Exp { root      :: CrossCheck a => a
        , arguments :: [Arg]
        }

instance Show Exp where
  show (Exp { arguments }) = show arguments


data Arg
  = forall a . (Typeable a, Data a) => ArgConstr (Proxy a) Constr [Arg]
  | forall a . (Typeable a, Data a) => ArgUndefined (Proxy a) Path
  | forall a . (Eq a, Show a, Typeable a) => ArgPrimitive a

instance Show Arg where
  show (ArgConstr _ c as)   = unwords [show c, show as]
  show (ArgUndefined _ p) = unwords ["?", show p]
  show (ArgPrimitive p)   = show p

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
refine primitives (Exp root args) path
  = [ Exp root args' | args' <- refineArgs args path]
  where
    refineArgs :: [Arg] -> Path -> [[Arg]]
    refineArgs args (Path (NENil p))
      = [ preArgs ++ arg':postArgs | arg'<- newArgs]
      where
        (preArgs, arg:postArgs)     = splitAt p args
        newArgs = refineOne primitives arg

    refineArgs args (Path (NECons p ps))
      = [ preArgs ++ arg':postArgs | arg' <- concat newArgs]
      where
        (preArgs, arg:postArgs)     = splitAt p args
        newArgs = case arg of
                    (ArgConstr poxy ctr args)
                      -> [ preArgs ++ (ArgConstr poxy ctr args') : postArgs
                         | args' <- refineArgs args (Path ps)
                         ]
                    _ -> error "refineArgs"

refineOne :: [Primitives] -> Arg -> [Arg]
refineOne primitives (ArgConstr _ ctr args)  = error "Refine a constructor?"
refineOne primitives (ArgUndefined (proxy :: Proxy a) (Path path))
  | null foundPrimitives
  = [ ArgConstr proxy c [ ArgUndefined proxy' $ Path (path `neSnoc` i)
                        | (i, DataExists proxy') <- [0 .. ] `zip`
                                                      childProxies proxy c
                        ]
    | c <- getConstructors proxy
    ]
  | otherwise
  = foundPrimitives
  where
    foundPrimitives = findPrimitives primitives (typeOf (undefined :: a))
                                     ArgPrimitive

refineOne primitives (ArgPrimitive _) = error "Refine a primitive?"

getConstructors :: forall a . Data a => Proxy a -> [Constr]
getConstructors _ = dataTypeConstrs $ dataTypeOf (undefined :: a)

childProxies :: forall a . Data a => Proxy a -> Constr -> [DataExists]
childProxies _ = gmapQ wrapUp . (fromConstr :: Constr -> a)

wrapUp :: forall d . Data d => d -> DataExists
wrapUp _ = DataExists (undefined :: Proxy d)

evalArg :: (Data a, Typeable a) => Arg -> a
evalArg (ArgConstr _ c as) = flip evalState as $ do
  fromConstrM (evalArg <$> next) c
  where
    next = state (\(x:xs) -> (x, xs))
evalArg (ArgUndefined _ path) = throw path
evalArg (ArgPrimitive prim) = fromMaybe (error "evalArg Prim cast") $ cast prim

applyArg :: (Typeable a, Data a) => (a -> b) -> Arg -> b
applyArg f arg = let a = evalArg arg in f a

initialExp :: CrossCheck a => a -> Exp
initialExp f = Exp { root = f
                   , arguments = [ ArgUndefined proxy (Path (NENil i)) |
                                    (i, DataExists proxy) <- [0 .. ] `zip`
                                                             argDatas f
                                    ]
                   }

class (Eq (Res a), Show (Res a), Typeable (Res a)) => CrossCheck a where
  type Res a
  argReps     :: a -> [(TypeRep, DataType)]
  apply       :: a -> [Arg] -> Res a
  argDatas    :: a -> [DataExists]

data DataExists = forall d . Data d => DataExists (Proxy d)

instance (Typeable a, Data a, CrossCheck b) => CrossCheck (a -> b) where
  type Res (a -> b) = Res b
  argDatas (_ :: a -> b) = DataExists (undefined :: Proxy a) :
                            argDatas (undefined :: b)
  argReps (_ :: a -> b) = (typeOf (undefined :: a), dataTypeOf (undefined :: a)) : argReps (undefined :: b)
  apply f (a:as) = (f `applyArg` a) `apply` as

instance (Eq a, Typeable a, Show a) => CrossCheck (Result a) where
  type Res (Result a) = a

  argReps _ = []
  argDatas _ = []

  apply (Result x) [] = x

