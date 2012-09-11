{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module LazyCrossCheck.Utils where

import Data.Data
import LazyCrossCheck.Primitives

getConstructors :: forall a . Data a => Proxy a -> [Constr]
getConstructors _ = dataTypeConstrs $ dataTypeOf (undefined :: a)

childProxies :: forall a . Data a => Proxy a -> Constr -> [DataExists]
childProxies _ = gmapQ wrapUp . (fromConstr :: Constr -> a)

wrapUp :: forall d . Data d => d -> DataExists
wrapUp _ = DataExists (undefined :: Proxy d)

data DataExists = forall d . Data d => DataExists (Proxy d)
