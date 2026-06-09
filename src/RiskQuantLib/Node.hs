module RiskQuantLib.Node (
  Node,
  new',
  new,
  lookupWith,
  getByKey',
  getByKey,
  hasByKey',
  hasByKey,
  setByKey',
  setByKey,
  get',
  get,
  has',
  has,
  set',
  set,
  getMaybeByKey',
  getMaybeByKey,
  getUnsafeByKey',
  getUnsafeByKey,
  (./),
  (.>),
  (../),
  (..>),
  (.<),
  (.?)
) where

import qualified RiskQuantLib.AttributeKey as AK

import qualified Data.Maybe as M
import qualified Data.IntMap.Strict as IMap
import qualified Control.Concurrent.STM as STM


type Node a = STM.TVar (IMap.IntMap a)

new' :: STM.STM (Node a)
new' = STM.newTVar IMap.empty

new :: IO (Node a)
new = STM.newTVarIO IMap.empty

lookupWith :: IMap.Key -> IMap.IntMap a -> (a -> b) -> b -> b
lookupWith k im func def = case IMap.lookup k im of
  Nothing -> def
  Just v -> func v

getByKey' :: Node a -> IMap.Key -> a -> STM.STM a
getByKey' n ak def = STM.readTVar n >>= \am -> return (lookupWith ak am id def)

getByKey :: Node a -> IMap.Key -> a -> IO a
getByKey n ak def = STM.readTVarIO n >>= \am -> return (lookupWith ak am id def)

hasByKey' :: Node a -> IMap.Key -> STM.STM Bool
hasByKey' n ak = STM.readTVar n >>= \am -> return (lookupWith ak am (\_ -> True) False)

hasByKey :: Node a -> IMap.Key -> IO Bool
hasByKey n ak = STM.readTVarIO n >>= \am -> return (lookupWith ak am (\_ -> True) False)

setByKey' :: Node a -> IMap.Key -> a -> STM.STM ()
setByKey' n ak v = do
  am <- STM.readTVar n
  STM.writeTVar n $ IMap.insert ak v am

setByKey :: Node a -> IMap.Key -> a -> IO ()
setByKey n ak v = STM.atomically $ setByKey' n ak v

get' :: Node a -> AK.AttrName -> a -> STM.STM a
get' n an def = getByKey' n (AK.toAttr an) def

get :: Node a -> AK.AttrName -> a -> IO a
get n an def = getByKey n (AK.toAttr an) def

has' :: Node a -> AK.AttrName -> STM.STM Bool
has' n an = hasByKey' n (AK.toAttr an)

has :: Node a -> AK.AttrName -> IO Bool
has n an = hasByKey n (AK.toAttr an)

set' :: Node a -> AK.AttrName -> a -> STM.STM ()
set' n an v = setByKey' n (AK.toAttr an) v 

set :: Node a -> AK.AttrName -> a -> IO ()
set n an v = setByKey n (AK.toAttr an) v 

getMaybeByKey' :: Node a -> IMap.Key -> STM.STM (Maybe a)
getMaybeByKey' n ak = STM.readTVar n >>= return . (IMap.lookup ak)

getMaybeByKey :: Node a -> IMap.Key -> IO (Maybe a)
getMaybeByKey n ak = STM.readTVarIO n >>= return . (IMap.lookup ak)

getUnsafeByKey' :: Node a -> IMap.Key -> STM.STM a
getUnsafeByKey' n ak = getMaybeByKey' n ak >>= return . M.fromJust

getUnsafeByKey :: Node a -> IMap.Key -> IO a
getUnsafeByKey n ak = getMaybeByKey n ak >>= return . M.fromJust

(./) :: Node a -> AK.AttrName -> STM.STM (Maybe a)
(./) n an = getMaybeByKey' n (AK.toAttr an)

(.>) :: Node a -> AK.AttrName -> IO (Maybe a)
(.>) n an = getMaybeByKey n (AK.toAttr an)

(../) :: Node a -> AK.AttrName -> STM.STM a
(../) n an = getUnsafeByKey' n (AK.toAttr an)

(..>) :: Node a -> AK.AttrName -> IO a
(..>) n an = getUnsafeByKey n (AK.toAttr an)

(.<) :: Node a -> AK.AttrName -> a -> IO ()
(.<) n an v = setByKey n (AK.toAttr an) v

(.?) :: Node a -> AK.AttrName -> IO Bool
(.?) n an = hasByKey n (AK.toAttr an)
