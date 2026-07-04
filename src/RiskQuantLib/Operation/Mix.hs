{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Operation.Mix (
  len,
  RiskQuantLib.Operation.Mix.null,
  (.!),
  (.>),
  (..>),
  (.<),
  (.?),
  empty,
  RiskQuantLib.Operation.Mix.elem,
  append,
  appendList,
  iLocV,
  iLoc,
  RiskQuantLib.Operation.Mix.head,
  RiskQuantLib.Operation.Mix.take,
  RiskQuantLib.Operation.Mix.last,
  lastN,
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
  RiskQuantLib.Operation.Mix.filter,
  filterP,
  filterPN,
  RiskQuantLib.Operation.Mix.zip,
  RiskQuantLib.Operation.Mix.zipWith,
  RiskQuantLib.Operation.Mix.zipWithM,
  RiskQuantLib.Operation.Mix.map,
  RiskQuantLib.Operation.Mix.mapM,
  apply,
  applyM,
  sortBy,
  sortUBy,
  groupBy,
  groupUBy,
  join,
  joinPN,
  connect,
  connectPN
) where

import qualified RiskQuantLib.Operation.Vector as OPV
import qualified RiskQuantLib.Operation.Graph as OG
import qualified RiskQuantLib.Attribute.Key as AK
import qualified RiskQuantLib.Attribute.Value as AV
import qualified RiskQuantLib.Node.Node as N
import qualified RiskQuantLib.Node.NodeVector as NV

import qualified Data.Vector.Strict as V
import qualified Data.Vector.Unboxed as VU
import qualified Control.Concurrent.Async as ANC

len :: OG.Graph -> Int
len (AV.Series v) = V.length v
len (AV.NodeList (_, nvc)) = NV.len nvc
len _ = 0

null :: OG.Graph -> Bool
null (AV.Series v) = V.null v
null (AV.NodeList (_, nvc)) = V.null nvc
null _ = True

(.!) :: OG.Graph -> NV.NodeIndex -> OG.Graph
(.!) (AV.Series sr) idx = OPV.iLocN sr idx
(.!) (AV.NodeList (_, nvc)) idx = AV.Node $ OPV.iLocN nvc idx
(.!) _ _ = AV.attrValueNan

(.>) :: OG.Graph -> AK.AttrName -> IO (Maybe OG.Graph)
(.>) (AV.NodeList (n, _)) attr = n N..> attr
(.>) (AV.Node n) attr = n N..> attr
(.>) _ _ = return Nothing

(..>) :: OG.Graph -> AK.AttrName -> IO OG.Graph
(..>) g attr = g .> attr >>= \r -> case r of
  Just v -> return v
  Nothing -> return AV.attrValueNan

(.<) :: OG.Graph -> AK.AttrName -> OG.Graph -> IO ()
(.<) (AV.NodeList (n, _)) attr v = n N..< attr $ v
(.<) (AV.Node n) attr v = n N..< attr $ v
(.<) _ _ _ = return ()

(.?) :: OG.Graph -> AK.AttrName -> IO Bool
(.?) (AV.NodeList (n, _)) attr = n N..? attr
(.?) (AV.Node n) attr = n N..? attr
(.?) _ _ = return False

empty :: OG.Graph -> Bool
empty (AV.Series v) = V.length v == 0
empty (AV.NodeList (_, nvc)) = NV.empty nvc
empty _ = True

elem :: OG.Graph -> OG.Graph -> Bool
elem n (AV.Series v) = V.elem n v
elem (AV.Node n) (AV.NodeList (_, nvc)) = NV.elem n nvc
elem _ _ = False

append :: OG.Graph -> OG.Graph -> OG.Graph
append (AV.Series sr) g = AV.Series $ V.snoc sr g
append (AV.NodeList (q, nvc)) (AV.Node n) = AV.NodeList (q, NV.append nvc n)
append _ _ = AV.attrValueNan

appendList :: OG.Graph -> [OG.Graph] -> OG.Graph
appendList (AV.Series sr) gs = AV.Series $ sr V.++ (V.fromList gs)
appendList nl [] = nl
appendList nl (n:ns) = appendList (append nl n) ns

iLocV :: OG.Graph -> V.Vector NV.NodeIndex -> OG.Graph
iLocV (AV.Series sr) il = AV.Series $ V.backpermute sr il
iLocV (AV.NodeList (q, nvc)) il = AV.NodeList (q, NV.iLocV nvc il)
iLocV _ _ = AV.attrValueNan

iLoc :: OG.Graph -> [NV.NodeIndex] -> OG.Graph
iLoc (AV.Series sr) il = AV.Series $ V.backpermute sr (V.fromList il)
iLoc (AV.NodeList (q, nvc)) il = AV.NodeList (q, NV.iLoc nvc il)
iLoc _ _ = AV.attrValueNan

take :: Int -> OG.Graph -> OG.Graph
take num (AV.Series sr) = AV.Series $ V.take num sr
take num (AV.NodeList (q, nvc)) = AV.NodeList (q, V.take num nvc)
take _ _ = AV.attrValueNan

head :: OG.Graph -> OG.Graph
head (AV.Series sr) = V.head sr
head (AV.NodeList (_, nvc)) = AV.Node $ V.head nvc
head _ = AV.attrValueNan

last :: OG.Graph -> OG.Graph
last (AV.Series sr) = V.last sr
last (AV.NodeList (_, nvc)) = AV.Node $ V.last nvc
last _ = AV.attrValueNan

lastN :: Int -> OG.Graph -> OG.Graph
lastN num (AV.Series sr) = AV.Series $ V.drop (V.length sr - num) sr
lastN num (AV.NodeList (q, nvc)) = AV.NodeList (q, V.drop (V.length nvc - num) nvc)
lastN _ _ = AV.attrValueNan

enumerate :: OG.Graph -> (NV.NodeIndex -> OG.Action b) -> IO (V.Vector b)
enumerate (AV.Series sr) func = V.imapM func sr
enumerate (AV.NodeList (_, nvc)) func = NV.enumerate nvc $ \i n -> func i (AV.Node n)
enumerate _ _ = return V.empty

enumerate_ :: OG.Graph -> (NV.NodeIndex -> OG.Action b) -> IO ()
enumerate_ (AV.Series sr) func = V.imapM_ func sr
enumerate_ (AV.NodeList (_, nvc)) func = NV.enumerate_ nvc $ \i n -> func i (AV.Node n)
enumerate_ _ _ = return ()

for :: OG.Graph -> OG.Action b -> IO (V.Vector b)
for (AV.Series sr) func = V.mapM func sr
for (AV.NodeList (_, nvc)) func = NV.for nvc $ \n -> func (AV.Node n)
for _ _ = return V.empty

for_ :: OG.Graph -> OG.Action b -> IO ()
for_ (AV.Series sr) func = V.mapM_ func sr
for_ (AV.NodeList (_, nvc)) func = NV.for_ nvc $ \n -> func (AV.Node n)
for_ _ _ = return ()

parallel :: OG.Graph -> OG.Action b -> IO (V.Vector b)
parallel (AV.Series sr) func = ANC.mapConcurrently func sr
parallel (AV.NodeList (_, nvc)) func = NV.parallel nvc $ \n -> func (AV.Node n)
parallel _ _ = return V.empty

parallel_ :: OG.Graph -> OG.Action b -> IO ()
parallel_ (AV.Series sr) func = ANC.mapConcurrently_ func sr
parallel_ (AV.NodeList (_, nvc)) func = NV.parallel_ nvc $ \n -> func (AV.Node n)
parallel_ _ _ = return ()

parallelN :: Int -> OG.Graph -> OG.Action b -> IO (V.Vector b)
parallelN num (AV.Series sr) func = ANC.mapConcurrently (\srb -> V.mapM func srb) (OPV.splitTo sr num) >>= return . V.concat
parallelN num (AV.NodeList (_, nvc)) func = NV.parallelN num nvc $ \n -> func (AV.Node n)
parallelN _ _ _ = return V.empty

parallelN_ :: Int -> OG.Graph -> OG.Action b -> IO ()
parallelN_ num (AV.Series sr) func = ANC.mapConcurrently_ (\srb -> V.mapM_ func srb) (OPV.splitTo sr num)
parallelN_ num (AV.NodeList (_, nvc)) func = NV.parallelN_ num nvc $ \n -> func (AV.Node n)
parallelN_ _ _ _ = return ()

add :: OG.Graph -> OG.Graph -> OG.Graph
add (AV.Series srA) (AV.Series srB) = AV.Series (srA V.++ srB)
add (AV.NodeList (qA, nvcA)) (AV.NodeList (_, nvcB)) = AV.NodeList (qA, NV.add nvcA nvcB)
add _ _ = AV.attrValueNan

sub :: OG.Graph -> OG.Graph -> OG.Graph
sub (AV.Series srA) (AV.Series srB) = AV.Series (V.filter (\n -> V.notElem n srB) srA)
sub (AV.NodeList (qA, nvcA)) (AV.NodeList (_, nvcB)) = AV.NodeList (qA, NV.sub nvcA nvcB)
sub _ _ = AV.attrValueNan

filter :: OG.Graph -> OG.Action Bool -> IO OG.Graph
filter (AV.Series sr) func = V.filterM func sr >>= return . AV.Series
filter (AV.NodeList (q, nvc)) func = NV.filter nvc (\n -> func (AV.Node n)) >>= \f -> return $ AV.NodeList (q, f)
filter _ _ = return AV.attrValueNan

filterP :: OG.Graph -> OG.Action Bool -> IO OG.Graph
filterP g@(AV.Series sr) func = parallel g func >>= \bool -> return . AV.Series . snd . V.unzip $ V.filter (\(b, _) -> b) (V.zip bool sr) 
filterP (AV.NodeList (q, nvc)) func = NV.filterP nvc (\n -> func (AV.Node n)) >>= \f -> return $ AV.NodeList (q, f)
filterP _ _ = return AV.attrValueNan

filterPN :: Int -> OG.Graph -> OG.Action Bool -> IO OG.Graph
filterPN num g@(AV.Series sr) func = parallelN num g func >>= \bool -> return . AV.Series . snd . V.unzip $ V.filter (\(b, _) -> b) (V.zip bool sr) 
filterPN num (AV.NodeList (q, nvc)) func = NV.filterPN num nvc (\n -> func (AV.Node n)) >>= \f -> return $ AV.NodeList (q, f)
filterPN _ _ _ = return AV.attrValueNan

zip :: OG.Graph -> OG.Graph -> V.Vector (OG.Graph, OG.Graph)
zip (AV.Series srA) (AV.Series srB) = V.zip srA srB
zip (AV.NodeList (_, nvcA)) (AV.NodeList (_, nvcB)) = V.zipWith (\i j -> (AV.Node i, AV.Node j)) nvcA nvcB
zip _ _ = V.empty

zipWith :: (OG.Graph -> OG.Graph -> a) -> OG.Graph -> OG.Graph -> V.Vector a
zipWith func (AV.Series srA) (AV.Series srB) = V.zipWith func srA srB
zipWith func (AV.NodeList (_, nvcA)) (AV.NodeList (_, nvcB)) = V.zipWith (\i j -> func (AV.Node i) (AV.Node j)) nvcA nvcB
zipWith _ _ _ = V.empty

zipWithM :: OG.Relation a -> OG.Graph -> OG.Graph -> IO (V.Vector a)
zipWithM func (AV.Series srA) (AV.Series srB) = V.zipWithM func srA srB
zipWithM func (AV.NodeList (_, nvcA)) (AV.NodeList (_, nvcB)) = V.zipWithM (\i j -> func (AV.Node i) (AV.Node j)) nvcA nvcB
zipWithM _ _ _ = return V.empty

map :: (OG.Graph -> a) -> OG.Graph -> V.Vector a
map func (AV.Series sr) = V.map func sr
map func (AV.NodeList (_, nvc)) = V.map (\n -> func $ AV.Node n) nvc
map _ _ = V.empty

mapM :: OG.Action a -> OG.Graph -> IO (V.Vector a)
mapM func (AV.Series sr) = V.mapM func sr
mapM func (AV.NodeList (_, nvc)) = V.mapM (\n -> func $ AV.Node n) nvc
mapM _ _ = return V.empty

apply :: (OG.Graph -> OG.Graph) -> OG.Graph -> OG.Graph
apply func g = AV.Series $ RiskQuantLib.Operation.Mix.map func g

applyM :: (OG.Graph -> IO OG.Graph) -> OG.Graph -> IO OG.Graph
applyM func g = RiskQuantLib.Operation.Mix.mapM func g >>= return . AV.Series

{-# INLINABLE sortBy #-}
sortBy :: (Ord b) => OG.Graph -> OG.Action b -> Bool -> IO OG.Graph
sortBy (AV.NodeList (q, nvc)) func ascending = NV.sortBy nvc (\n -> func $ AV.Node n) ascending >>= \f -> return $ AV.NodeList (q, f)
sortBy (AV.Series sr) func ascending = V.mapM func sr >>= \v -> return . AV.Series $ OPV.argSortBy v (\i -> sr V.! i) ascending
sortBy _ _ _ = return AV.attrValueNan

{-# INLINABLE sortUBy #-}
sortUBy :: forall b. (Ord b, VU.Unbox b) => OG.Graph -> (OG.Graph -> IO b) -> Bool -> IO OG.Graph
sortUBy (AV.NodeList (q, nvc)) func ascending = NV.sortUBy nvc (\n -> func $ AV.Node n) ascending >>= \f -> return $ AV.NodeList (q, f)
sortUBy (AV.Series sr) func ascending = V.mapM func sr >>= \v -> return . AV.Series $ V.backpermute sr $ V.convert $ OPV.argSort (V.convert v :: VU.Vector b) ascending
sortUBy _ _ _ = return AV.attrValueNan

{-# INLINABLE groupBy #-}
groupBy :: Ord b => OG.Graph -> OG.Action b -> IO OG.Graph
groupBy (AV.NodeList (_, nvc)) func = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupBy nvc (\i -> func $ AV.Node i)) >>= return . AV.Series
groupBy (AV.Series sr) func = V.mapM func sr >>= \v -> do
  let idx = OPV.argSort v True
  let ss = V.backpermute sr idx
  let vs = V.backpermute v idx
  let vg = OPV.group vs
  let lens = V.map V.length vg
  let offsets = V.prescanl' (+) 0 lens
  return . AV.Series $ V.zipWith (\off l -> AV.Series $ V.slice off l ss) offsets lens
groupBy _ _ = return AV.attrValueNan

{-# INLINABLE groupUBy #-}
groupUBy :: forall b. (Ord b, VU.Unbox b) => OG.Graph -> OG.Action b -> IO OG.Graph
groupUBy (AV.NodeList (_, nvc)) func = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupUBy nvc (\i -> func $ AV.Node i)) >>= return . AV.Series
groupUBy (AV.Series sr) func = V.mapM func sr >>= \v -> do
  let idx = V.convert $ OPV.argSort (V.convert v :: VU.Vector b) True
  let ss = V.backpermute sr idx
  let vs = V.backpermute v idx
  let vg = OPV.group vs
  let lens = V.map V.length vg
  let offsets = V.prescanl' (+) 0 lens
  return . AV.Series $ V.zipWith (\off l -> AV.Series $ V.slice off l ss) offsets lens
groupUBy _ _ = return AV.attrValueNan

join :: OG.Graph -> OG.Graph -> AK.AttrName -> OG.Relation Bool -> IO ()
join gA gB attrA func = for_ gA $ \ia -> RiskQuantLib.Operation.Mix.filter gB (func ia) >>= ia .< attrA

joinPN :: Int -> OG.Graph -> OG.Graph -> AK.AttrName -> OG.Relation Bool -> IO ()
joinPN num gA gB attr func = parallelN_ num gA $ \ia -> RiskQuantLib.Operation.Mix.filter gB (func ia) >>= ia .< attr

connect :: OG.Graph -> OG.Graph -> AK.AttrName -> AK.AttrName -> OG.Relation Bool -> OG.Relation Bool -> IO ()
connect gA gB attrA attrB funcA funcB = ANC.concurrently_ joinLT joinRT
  where
    joinLT = join gA gB attrA funcA
    joinRT = join gB gA attrB $ flip funcB

connectPN :: Int -> OG.Graph -> OG.Graph -> AK.AttrName -> AK.AttrName -> OG.Relation Bool -> OG.Relation Bool -> IO ()
connectPN num gA gB attrA attrB funcA funcB = ANC.concurrently_ joinLT joinRT
  where
    joinLT = joinPN num gA gB attrA funcA
    joinRT = joinPN num gB gA attrB $ flip funcB
