{-# LANGUAGE ScopedTypeVariables #-}
module LazyCrossCheck.Expressions where

import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Data
import Data.Maybe

import LazyCrossCheck.Eval
import LazyCrossCheck.NEList
import LazyCrossCheck.Primitives
import LazyCrossCheck.Types
import LazyCrossCheck.Utils


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

refineOne :: [Primitives] -> Arg -> [Arg]
refineOne _ (ArgConstr _ _ _)  = error "Refine a constructor?"
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

refineOne _ (ArgPrimitive _) = error "Refine a primitive?"

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

