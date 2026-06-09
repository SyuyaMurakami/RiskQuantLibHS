{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.NodeVector (
  NodeIndex,
  NodeVector,
  new,
  newN,
  len,
  empty,
  RiskQuantLib.NodeVector.elem,
  append,
  appendList,
  iLoc,
  iLocV,
  iLocN,
  enumerate,
  enumerate_,
  for,
  for_,
  parallel,
  parallel_,
  parallelN,
  parallelN_,
  add,
  sub,
  RiskQuantLib.NodeVector.filter,
  filterP,
  filterPN,
  setV,
  set,
  get,
  getPN,
  getS,
  has,
  hasPN,
  stepBy,
  reduce,
  RiskQuantLib.NodeVector.sum,
  prod,
  RiskQuantLib.NodeVector.min,
  RiskQuantLib.NodeVector.max,
  mean,
  cumReduce,
  cumSum,
  cumProd,
  cumMin,
  cumMax,
  rolling,
  rollingCenter,
  sort,
  sortBy,
  sortU,
  sortUBy,
  sortPN,
  sortS,
  groupCoreBy,
  groupCore,
  groupBy,
  group,
  groupUBy,
  groupU,
  groupPN,
  groupS
) where

import qualified RiskQuantLib.Algorithms as VAG
import qualified RiskQuantLib.AttributeKey as AK
import qualified RiskQuantLib.Node as N

import qualified Data.Vector.Strict as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Control.Concurrent.Async as ANC

type NodeIndex = Int
type NodeVector a = V.Vector (N.Node a)

new :: NodeVector a
new = V.empty

newN :: Int -> IO (NodeVector a)
newN num = V.generateM num $ \_ -> N.new

len :: NodeVector a -> Int
len = V.length

empty :: NodeVector a -> Bool
empty nvc = if V.length nvc == 0 then True else False

elem :: N.Node a -> NodeVector a -> Bool
elem n nvc = V.elem n nvc

append :: NodeVector a -> N.Node a -> NodeVector a
append nvc n = V.snoc nvc n

appendList :: NodeVector a -> [N.Node a] -> NodeVector a
appendList nvc ns = nvc V.++ (V.fromList ns)

iLoc :: NodeVector a -> NodeIndex -> N.Node a
iLoc nvc i = nvc V.! i

iLocV ::  NodeVector a -> V.Vector NodeIndex -> NodeVector a
iLocV nvc il = V.backpermute nvc il

iLocN ::  NodeVector a -> [NodeIndex] -> NodeVector a
iLocN nvc il = iLocV nvc $ V.fromList il

enumerate :: NodeVector a -> (NodeIndex -> N.Node a -> IO b) -> IO (V.Vector b)
enumerate nvc func = V.imapM func nvc

enumerate_ :: NodeVector a -> (NodeIndex -> N.Node a -> IO b) -> IO ()
enumerate_ nvc func = V.imapM_ func nvc

for :: NodeVector a -> (N.Node a -> IO b) -> IO (V.Vector b)
for nvc func = V.forM nvc func

for_ :: NodeVector a -> (N.Node a -> IO b) -> IO ()
for_ nvc func = V.forM_ nvc func

parallel :: NodeVector a -> (N.Node a -> IO b) -> IO (V.Vector b)
parallel nvc func = ANC.mapConcurrently func nvc

parallel_ :: NodeVector a -> (N.Node a -> IO b) -> IO ()
parallel_ nvc func = ANC.mapConcurrently_ func nvc

parallelN :: Int -> NodeVector a -> (N.Node a -> IO b) -> IO (V.Vector b)
parallelN num nvc func = ANC.mapConcurrently funcBlock nvcBlock >>= return . V.concat
  where
    funcBlock = \nvcb -> for nvcb func
    nvcBlock = VAG.splitTo nvc num

parallelN_ :: Int -> NodeVector a -> (N.Node a -> IO b) -> IO ()
parallelN_ num nvc func = ANC.mapConcurrently_ funcBlock nvcBlock
  where
    funcBlock = \nvcb -> for_ nvcb func
    nvcBlock = VAG.splitTo nvc num

add :: NodeVector a -> NodeVector a -> NodeVector a
add nvcA nvcB = nvcA V.++ nvcB

sub :: NodeVector a -> NodeVector a -> NodeVector a
sub nvcA nvcB = V.filter (\n -> V.notElem n nvcB) nvcA

filter :: NodeVector a -> (N.Node a -> IO Bool) -> IO (NodeVector a)
filter nvc func = V.filterM func nvc

filterP :: NodeVector a -> (N.Node a -> IO Bool) -> IO (NodeVector a)
filterP nvc func = parallel nvc func >>= \bool -> return . snd . V.unzip $ V.filter (\(b, _) -> b) (V.zip bool nvc)

filterPN :: Int -> NodeVector a -> (N.Node a -> IO Bool) -> IO (NodeVector a)
filterPN num nvc func = parallelN num nvc func >>= \bool -> return . snd . V.unzip $ V.filter (\(b, _) -> b) (V.zip bool nvc)

setV :: NodeVector a -> AK.AttrName -> V.Vector NodeIndex -> V.Vector a -> IO ()
setV nvc attr key value = do
  let !ak = AK.toAttr attr
  let !l = V.length nvc
  let !idx = V.filter (\(i, _) -> i >= 0 && i < l) $ V.zip key value
  V.forM_ idx $ \(i, v) -> N.setByKey (nvc V.! i) ak v

set :: NodeVector a -> AK.AttrName -> [NodeIndex] -> [a] -> IO ()
set nvc attr key value = do
  let !ak = AK.toAttr attr
  let !l = V.length nvc
  let !idx = V.filter (\(i, _) -> i >= 0 && i < l) $ V.fromList $ zip key value
  V.forM_ idx $ \(i, v) -> N.setByKey (nvc V.! i) ak v

get :: NodeVector a -> AK.AttrName -> a -> IO (V.Vector a)
get nvc attr def = let !ak = AK.toAttr attr in for nvc $ \n -> N.getByKey n ak def

getPN :: Int -> NodeVector a -> AK.AttrName -> a -> IO (V.Vector a)
getPN num nvc attr def = let !ak = AK.toAttr attr in parallelN num nvc $ \n -> N.getByKey n ak def

getS :: NodeVector a -> [AK.AttrName] -> a -> IO (V.Vector (V.Vector a))
getS nvc attrL def = ANC.mapConcurrently (\attr -> get nvc attr def) $ V.fromList attrL

has :: NodeVector a -> AK.AttrName -> IO (NodeVector a)
has nvc attr = let !ak = AK.toAttr attr in RiskQuantLib.NodeVector.filter nvc $ \n -> N.hasByKey n ak 

hasPN :: Int -> NodeVector a -> AK.AttrName -> IO (NodeVector a)
hasPN num nvc attr = let !ak = AK.toAttr attr in filterPN num nvc $ \n -> N.hasByKey n ak

stepBy :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
stepBy func accu ni = case ni of 
  Nothing -> accu
  Just v -> Just $ case accu of 
    Nothing -> v
    Just ac -> func ac v

reduce :: (a -> a -> a) -> NodeVector a -> AK.AttrName -> IO (Maybe a)
reduce func nvc attr = V.foldM' f Nothing nvc
  where
    !ak = AK.toAttr attr
    f accu n = do
      nv <- N.getMaybeByKey n ak
      return $ stepBy func accu nv

sum :: Num a => NodeVector a -> AK.AttrName -> IO (Maybe a)
sum nvc attr = reduce (+) nvc attr

prod :: Num a => NodeVector a -> AK.AttrName -> IO (Maybe a)
prod nvc attr = reduce (*) nvc attr

min :: Ord a => NodeVector a -> AK.AttrName -> IO (Maybe a)
min nvc attr = reduce Prelude.min nvc attr

max :: Ord a => NodeVector a -> AK.AttrName -> IO (Maybe a)
max nvc attr = reduce Prelude.max nvc attr

mean :: Fractional a => NodeVector a -> AK.AttrName -> IO (Maybe a)
mean nvc attr = V.foldM' func (0 :: Integer, 0) nvc >>= \(c, s) -> return $ if c == 0 then Nothing else Just (s / (fromIntegral c))
  where
    !ak = AK.toAttr attr
    func accu@(count, accuValue) n = do
      av <- N.getMaybeByKey n ak
      case av of 
        Nothing -> return accu
        Just v -> return (count + 1, accuValue + v)

cumReduce :: (a -> a -> a) -> NodeVector a -> AK.AttrName -> IO (V.Vector (Maybe a))
cumReduce func nvc attr = let !ak = AK.toAttr attr in for nvc (\n -> N.getMaybeByKey n ak) >>= \nv -> return $ V.prescanl' (stepBy func) Nothing nv

cumSum :: Num a => NodeVector a -> AK.AttrName -> IO (V.Vector (Maybe a))
cumSum nvc attr = cumReduce (+) nvc attr

cumProd :: Num a => NodeVector a -> AK.AttrName -> IO (V.Vector (Maybe a))
cumProd nvc attr = cumReduce (*) nvc attr

cumMin :: Ord a => NodeVector a -> AK.AttrName -> IO (V.Vector (Maybe a))
cumMin nvc attr = cumReduce Prelude.min nvc attr

cumMax :: Ord a => NodeVector a -> AK.AttrName -> IO (V.Vector (Maybe a))
cumMax nvc attr = cumReduce Prelude.max nvc attr

{-# INLINABLE rolling #-}
rolling :: NodeVector a -> Int -> V.Vector (NodeVector a)
rolling nvc window = VAG.rolling nvc window

{-# INLINABLE rollingCenter #-}
rollingCenter :: NodeVector a -> Int -> V.Vector (NodeVector a)
rollingCenter nvc window = VAG.rollingCenter nvc window

{-# INLINABLE sort #-}
sort :: Ord a => NodeVector a -> AK.AttrName -> Bool -> IO (NodeVector a)
sort nvc attr ascending = has nvc attr >>= \t -> let !ak = AK.toAttr attr in for t (\n -> N.getUnsafeByKey n ak) >>= \s -> return $ VAG.argSortBy s (\i -> t V.! i) ascending

{-# INLINABLE sortBy #-}
sortBy :: Ord b => NodeVector a -> (N.Node a -> IO b) -> Bool -> IO (NodeVector a)
sortBy nvc func ascending = for nvc func >>= \s -> return $ VAG.argSortBy s (\i -> nvc V.! i) ascending

{-# INLINABLE sortU #-}
sortU :: forall a. (Ord a, VU.Unbox a) => NodeVector a -> AK.AttrName -> Bool -> IO (NodeVector a)
sortU nvc attr ascending = has nvc attr >>= \t -> let !ak = AK.toAttr attr in for t (\n -> N.getUnsafeByKey n ak) >>= \s -> return $ V.backpermute nvc $ VG.convert $ VAG.argSort (VG.convert s :: VU.Vector a) ascending

{-# INLINABLE sortUBy #-}
sortUBy :: forall a b. (Ord b, VU.Unbox b) => NodeVector a -> (N.Node a -> IO b) -> Bool -> IO (NodeVector a)
sortUBy nvc func ascending = for nvc func >>= \s -> return $ V.backpermute nvc $ VG.convert $ VAG.argSort (VG.convert s :: VU.Vector b) ascending

{-# INLINABLE sortPN #-}
sortPN :: Ord a => Int -> NodeVector a -> AK.AttrName -> Bool -> IO (NodeVector a)
sortPN num nvc attr ascending = hasPN num nvc attr >>= \t -> let !ak = AK.toAttr attr in parallelN num t (\n -> N.getUnsafeByKey n ak) >>= \s -> return $ VAG.argSortBy s (\i -> t V.! i) ascending

{-# INLINABLE sortS #-}
sortS :: Ord a => NodeVector a -> [AK.AttrName] -> Bool -> IO (NodeVector a)
sortS nvc attrL ascending = ANC.mapConcurrently (\attr -> let !ak = AK.toAttr attr in for nvc $ \n -> N.getMaybeByKey n ak) attrL >>= \s -> return $ VAG.argSortByS s (\i -> nvc V.! i) ascending

{-# INLINABLE groupCoreBy #-}
groupCoreBy :: Ord b => NodeVector a -> (N.Node a -> IO b) -> (V.Vector b -> V.Vector NodeIndex) -> IO (V.Vector (NodeVector a))
groupCoreBy nvc calFunc sortFunc = do
  s <- for nvc calFunc
  let !idx = sortFunc s
  let !ns = V.backpermute nvc idx
  let !vs = V.backpermute s idx
  let !vg = VAG.group vs
  let !lens = V.map V.length vg
  let !offsets = V.prescanl' (+) 0 lens
  return $ V.zipWith (\off l -> V.slice off l ns) offsets lens

{-# INLINABLE groupCore #-}
groupCore :: Ord a => NodeVector a -> AK.AttrName -> (V.Vector a -> V.Vector NodeIndex) -> IO (V.Vector (NodeVector a))
groupCore nvc attr sortFunc = let !ak = AK.toAttr attr in has nvc attr >>= \t -> groupCoreBy t (\n -> N.getUnsafeByKey n ak) sortFunc

{-# INLINABLE groupBy #-}
groupBy :: Ord b => NodeVector a -> (N.Node a -> IO b) -> IO (V.Vector (NodeVector a))
groupBy nvc func = groupCoreBy nvc func $ \s -> VAG.argSort s True

{-# INLINABLE group #-}
group :: Ord a => NodeVector a -> AK.AttrName -> IO (V.Vector (NodeVector a))
group nvc attr = groupCore nvc attr $ \s -> VAG.argSort s True

{-# INLINABLE groupUBy #-}
groupUBy :: forall a b. (Ord b, VU.Unbox b) => NodeVector a -> (N.Node a -> IO b) -> IO (V.Vector (NodeVector a))
groupUBy nvc func = groupCoreBy nvc func $ \s -> VG.convert $ VAG.argSort (VG.convert s :: VU.Vector b) True

{-# INLINABLE groupU #-}
groupU :: forall a. (Ord a, VU.Unbox a) => NodeVector a -> AK.AttrName -> IO (V.Vector (NodeVector a))
groupU nvc attr = groupCore nvc attr $ \s -> VG.convert $ VAG.argSort (VG.convert s :: VU.Vector a) True

{-# INLINABLE groupPN #-}
groupPN :: Ord a => Int -> NodeVector a -> AK.AttrName -> IO (V.Vector (NodeVector a))
groupPN num nvc attr = do
  t <- hasPN num nvc attr
  let !ak = AK.toAttr attr
  s <- parallelN num t $ \n -> N.getUnsafeByKey n ak
  let !idx = VAG.argSort s True
  let !ns = V.backpermute t idx
  let !vs = V.backpermute s idx
  let !vg = VAG.group vs
  let !lens = V.map V.length vg
  let !offsets = V.prescanl' (+) 0 lens
  return $ V.zipWith (\off l -> V.slice off l ns) offsets lens

{-# INLINABLE groupS #-}
groupS :: Ord a => NodeVector a -> [AK.AttrName] -> IO (V.Vector (NodeVector a))
groupS nvc attrL = do
  s <- ANC.mapConcurrently (\attr -> let !ak = AK.toAttr attr in for nvc $ \n -> N.getMaybeByKey n ak) attrL
  let !idx = VAG.argSortS s True
  let !ns = V.backpermute nvc idx
  let !vs = map (\ss -> V.backpermute ss idx) s
  let !vg = V.groupBy (\i j -> VAG.compareS vs True i j == EQ) $ V.enumFromN 0 (len nvc)
  let !lens = V.map V.length $ V.fromList vg
  let !offsets = V.prescanl' (+) 0 lens
  return $ V.zipWith (\off l -> V.slice off l ns) offsets lens

