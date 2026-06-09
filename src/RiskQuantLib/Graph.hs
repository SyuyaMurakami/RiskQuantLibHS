{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Graph (
  Graph,
  Action,
  Relation,
  columnsAttr,
  readCSV,
  columns,
  mapAtAll,
  headN,
  transpose,
  toCSV,
  new,
  newN,
  (.!),
  (.>),
  (..>),
  (.<),
  (.?),
  get,
  has,
  set,
  setBy,
  getPN,
  getS,
  hasPN,
  len,
  empty,
  RiskQuantLib.Graph.elem,
  append,
  appendList,
  iLocV,
  iLoc,
  RiskQuantLib.Graph.take,
  RiskQuantLib.Graph.last,
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
  RiskQuantLib.Graph.filter,
  filterP,
  filterPN,
  reduce,
  RiskQuantLib.Graph.sum,
  prod,
  RiskQuantLib.Graph.min,
  RiskQuantLib.Graph.max,
  mean,
  cumReduce,
  cumSum,
  cumProd,
  cumMin,
  cumMax,
  rollingCore,
  rolling,
  rollingCenter,
  sort,
  sortBy,
  sortPN,
  sortS,
  sortUBy,
  group,
  groupBy,
  groupUBy,
  groupPN,
  groupS,
  unique,
  join,
  joinPN,
  connect,
  connectPN,
  fillNan,
  dropNan,
  isNan,
  notNan,
  whereG,
  select,
  RiskQuantLib.Graph.zipWith,
  RiskQuantLib.Graph.map,
  mapAt,
  mapAtS,
  toDict,
  RiskQuantLib.Graph.lookup,
  insert
) where

import qualified RiskQuantLib.Algorithms as VAG
import qualified RiskQuantLib.AttributeKey as AK
import qualified RiskQuantLib.AttributeValue as AV
import qualified RiskQuantLib.FileTool as FT
import qualified RiskQuantLib.Node as N
import qualified RiskQuantLib.NodeVector as NV

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashTable.IO as H
import qualified Control.Concurrent.Async as ANC

type Graph = AV.AttrValue
type Action a = Graph -> IO a
type Relation a = Graph -> Graph -> IO a

------------------------------------------ Advanced Operation -------------------------------------------------

columnsAttr :: AK.AttrName
columnsAttr = "columns"

readCSV :: FilePath -> IO Graph
readCSV path = FT.readFileWithUtf8 path >>= \content -> do
  let allLines = V.filter (not . T.null) (V.fromList . T.lines $ content)
  case V.length allLines == 0 of
    True -> N.new >>= \n -> return $ AV.NodeList (n, NV.new)
    False -> do
      let cells = V.map (\line -> V.fromList $ T.splitOn (T.pack ",") line) allLines
      let header = V.head cells
      let values = V.tail cells
      vec <- V.forM values $ \line -> do
        n <- N.new
        V.zipWithM_ (\attr v -> (N..<) n (T.unpack attr) (AV.fromText v)) header line
        return n
      n <- N.new
      (N..<) n columnsAttr (AV.Series $ V.map AV.fromText header)
      return $ AV.NodeList (n, vec)

columns :: Graph -> IO [AK.AttrName]
columns g@(AV.NodeList (_, _)) = do
  col <- g .> columnsAttr
  case col of
    Just c -> for c (return . T.unpack . AV.toText) >>= return . V.toList
    Nothing -> return []
columns _ = return []

mapAtAll :: (Graph -> Graph) -> Graph -> IO ()
mapAtAll func g@(AV.NodeList (_, _)) = do
  col <- g .> columnsAttr
  case col of
    Just (AV.Series sr) -> flip ANC.mapConcurrently_ sr $ \attr -> case attr of
      AV.Element (AV.ElemText tr) -> mapAt (T.unpack tr) func g
      _ -> return ()
    _ -> return ()
mapAtAll _ _ = return ()

headN :: Int -> Graph -> IO Graph
headN num g = columns g >>= \col -> g `iLoc` [0..(num-1)] `getS` col

transpose :: Graph -> Graph
transpose (AV.Series sr) = AV.Series . V.fromList $ Prelude.map AV.fromList rows
  where
    rowsNumV = V.map len sr
    rowsNum = V.foldl' Prelude.min (V.head rowsNumV) rowsNumV
    values = V.map (RiskQuantLib.Graph.take rowsNum) sr
    list = V.toList $ V.map AV.toList values
    rows = L.transpose list
transpose _ = AV.attrValueNan

toCSV :: FilePath -> [String] -> Graph -> IO ()
toCSV path headers (AV.Series sr)
  | V.length sr == 0 = return ()
  | otherwise = FT.writeFileEnsuringDir path finalContent
  where
    rowsNumV = V.map len sr
    rowsNum = V.foldl' Prelude.min (V.head rowsNumV) rowsNumV
    values = V.map (RiskQuantLib.Graph.take rowsNum) sr
    headerText = T.intercalate (T.pack ",") $ Prelude.map T.pack headers
    listCols = V.toList $ V.map AV.toList values
    rows = L.transpose listCols
    formatCell x = T.pack (show x)
    formatRow row = T.intercalate (T.pack ",") (Prelude.map formatCell row)
    allRows = headerText : Prelude.map formatRow rows
    finalContent = T.unlines allRows
toCSV _ _ _ = return ()

------------------------------------------ Basic Operation -------------------------------------------------

new :: IO Graph
new = ((, NV.new) <$> N.new) >>= return . AV.NodeList

newN :: Int -> IO Graph
newN num = ((,) <$> N.new <*> (NV.newN num)) >>= return . AV.NodeList

(.!) :: Graph -> NV.NodeIndex -> Graph
(.!) (AV.Series sr) idx = sr V.! idx
(.!) (AV.NodeList (_, nvc)) idx = AV.Node $ nvc V.! idx
(.!) _ _ = AV.attrValueNan

(.>) :: Graph -> AK.AttrName -> IO (Maybe Graph)
(.>) (AV.NodeList (n, _)) attr = n N..> attr
(.>) (AV.Node n) attr = n N..> attr
(.>) _ _ = return Nothing

(..>) :: Graph -> AK.AttrName -> IO Graph
(..>) g attr = g .> attr >>= \r -> case r of
  Just v -> return v
  Nothing -> return AV.attrValueNan

(.<) :: Graph -> AK.AttrName -> Graph -> IO ()
(.<) (AV.NodeList (n, _)) attr v = n N..< attr $ v
(.<) (AV.Node n) attr v = n N..< attr $ v
(.<) _ _ _ = return ()

(.?) :: Graph -> AK.AttrName -> IO Bool
(.?) (AV.NodeList (n, _)) attr = n N..? attr
(.?) (AV.Node n) attr = n N..? attr
(.?) _ _ = return False

get :: Graph -> AK.AttrName -> IO Graph
get (AV.NodeList (_, nvc)) attr = NV.get nvc attr AV.attrValueNan >>= return . AV.Series
get _ _ = return AV.attrValueNan

has :: Graph -> AK.AttrName -> IO Graph
has (AV.NodeList (q, nvc)) attr = NV.has nvc attr >>= \vb -> return $ AV.NodeList (q, vb)
has _ _ = new

set :: Graph -> AK.AttrName -> [NV.NodeIndex] -> [Graph] -> IO ()
set (AV.NodeList (_, nvc)) attr key value = NV.set nvc attr key value
set _ _ _ _ = return ()

setBy :: Graph -> AK.AttrName -> AK.AttrName -> AV.ElementDict -> IO ()
setBy (AV.NodeList (_, nvc)) attrKey attrValue dict = NV.for_ nvc $ \n -> do
  key <- n N..> attrKey
  case key of
    Just (AV.Element k) -> do
      mv <- H.lookup dict k
      case mv of
        Just v -> n N..< attrValue $ AV.Element v
        Nothing -> return ()
    _ -> return ()
setBy _ _ _ _ = return ()

getPN :: Int -> Graph -> AK.AttrName -> IO Graph
getPN num (AV.NodeList (_, nvc)) attr = NV.getPN num nvc attr AV.attrValueNan >>= return . AV.Series
getPN _ _ _ = return AV.attrValueNan

getS :: Graph -> [AK.AttrName] -> IO Graph
getS (AV.NodeList (_, nvc)) attrL = NV.getS nvc attrL AV.attrValueNan >>= \vl -> return . AV.Series $ V.map AV.Series vl
getS _ _ = return AV.attrValueNan

hasPN :: Int -> Graph -> AK.AttrName -> IO Graph
hasPN num (AV.NodeList (q, nvc)) attr = NV.hasPN num nvc attr >>= \vb -> return $ AV.NodeList (q, vb)
hasPN _ _ _ = new

len :: Graph -> Int
len (AV.Series v) = V.length v
len (AV.NodeList (_, nvc)) = NV.len nvc
len _ = 0

empty :: Graph -> Bool
empty (AV.Series v) = V.length v == 0
empty (AV.NodeList (_, nvc)) = NV.empty nvc
empty _ = True

elem :: Graph -> Graph -> Bool
elem n (AV.Series v) = V.elem n v
elem (AV.Node n) (AV.NodeList (_, nvc)) = NV.elem n nvc
elem _ _ = False

append :: Graph -> Graph -> Graph
append (AV.Series sr) g = AV.Series $ V.snoc sr g
append (AV.NodeList (q, nvc)) (AV.Node n) = AV.NodeList (q, NV.append nvc n)
append _ _ = AV.attrValueNan

appendList :: Graph -> [Graph] -> Graph
appendList (AV.Series sr) gs = AV.Series $ sr V.++ (V.fromList gs)
appendList nl [] = nl
appendList nl (n:ns) = appendList (append nl n) ns

iLocV ::  Graph -> V.Vector NV.NodeIndex -> Graph
iLocV (AV.Series sr) il = AV.Series $ V.backpermute sr il
iLocV (AV.NodeList (q, nvc)) il = AV.NodeList (q, NV.iLocV nvc il)
iLocV _ _ = AV.attrValueNan

iLoc ::  Graph -> [NV.NodeIndex] -> Graph
iLoc (AV.Series sr) il = AV.Series $ V.backpermute sr (V.fromList il)
iLoc (AV.NodeList (q, nvc)) il = AV.NodeList (q, NV.iLocN nvc il)
iLoc _ _ = AV.attrValueNan

take :: Int -> Graph -> Graph
take num (AV.Series sr) = AV.Series $ V.take num sr
take num (AV.NodeList (q, nvc)) = AV.NodeList (q, V.take num nvc)
take _ _ = AV.attrValueNan

last :: Graph -> Graph
last (AV.Series sr) = V.last sr
last (AV.NodeList (_, nvc)) = AV.Node $ V.last nvc
last _ = AV.attrValueNan

lastN :: Int -> Graph -> Graph
lastN num (AV.Series sr) = AV.Series $ V.drop (V.length sr - num) sr
lastN num (AV.NodeList (q, nvc)) = AV.NodeList (q, V.drop (V.length nvc - num) nvc)
lastN _ _ = AV.attrValueNan

enumerate :: Graph -> (NV.NodeIndex -> Action b) -> IO (V.Vector b)
enumerate (AV.Series sr) func = V.imapM func sr
enumerate (AV.NodeList (_, nvc)) func = NV.enumerate nvc $ \i n -> func i (AV.Node n)
enumerate _ _ = return V.empty

enumerate_ :: Graph -> (NV.NodeIndex -> Action b) -> IO ()
enumerate_ (AV.Series sr) func = V.imapM_ func sr
enumerate_ (AV.NodeList (_, nvc)) func = NV.enumerate_ nvc $ \i n -> func i (AV.Node n)
enumerate_ _ _ = return ()

for :: Graph -> Action b -> IO (V.Vector b)
for (AV.Series sr) func = V.mapM func sr
for (AV.NodeList (_, nvc)) func = NV.for nvc $ \n -> func (AV.Node n)
for _ _ = return V.empty

for_ :: Graph -> Action b -> IO ()
for_ (AV.Series sr) func = V.mapM_ func sr
for_ (AV.NodeList (_, nvc)) func = NV.for_ nvc $ \n -> func (AV.Node n)
for_ _ _ = return ()

parallel :: Graph -> Action b -> IO (V.Vector b)
parallel (AV.Series sr) func = ANC.mapConcurrently func sr
parallel (AV.NodeList (_, nvc)) func = NV.parallel nvc $ \n -> func (AV.Node n)
parallel _ _ = return V.empty

parallel_ :: Graph -> Action b -> IO ()
parallel_ (AV.Series sr) func = ANC.mapConcurrently_ func sr
parallel_ (AV.NodeList (_, nvc)) func = NV.parallel_ nvc $ \n -> func (AV.Node n)
parallel_ _ _ = return ()

parallelN :: Int -> Graph -> Action b -> IO (V.Vector b)
parallelN num (AV.Series sr) func = ANC.mapConcurrently (\srb -> V.mapM func srb) (VAG.splitTo sr num) >>= return . V.concat
parallelN num (AV.NodeList (_, nvc)) func = NV.parallelN num nvc $ \n -> func (AV.Node n)
parallelN _ _ _ = return V.empty

parallelN_ :: Int -> Graph -> Action b -> IO ()
parallelN_ num (AV.Series sr) func = ANC.mapConcurrently_ (\srb -> V.mapM_ func srb) (VAG.splitTo sr num)
parallelN_ num (AV.NodeList (_, nvc)) func = NV.parallelN_ num nvc $ \n -> func (AV.Node n)
parallelN_ _ _ _ = return ()

add :: Graph -> Graph -> Graph
add (AV.Series srA) (AV.Series srB) = AV.Series (srA V.++ srB)
add (AV.NodeList (qA, nvcA)) (AV.NodeList (_, nvcB)) = AV.NodeList (qA, NV.add nvcA nvcB)
add _ _ = AV.attrValueNan

sub :: Graph -> Graph -> Graph
sub (AV.Series srA) (AV.Series srB) = AV.Series (V.filter (\n -> V.notElem n srB) srA)
sub (AV.NodeList (qA, nvcA)) (AV.NodeList (_, nvcB)) = AV.NodeList (qA, NV.sub nvcA nvcB)
sub _ _ = AV.attrValueNan

filter :: Graph -> Action Bool -> IO Graph
filter (AV.Series sr) func = V.filterM func sr >>= return . AV.Series
filter (AV.NodeList (q, nvc)) func = NV.filter nvc (\n -> func (AV.Node n)) >>= \f -> return $ AV.NodeList (q, f)
filter _ _ = return AV.attrValueNan

filterP :: Graph -> Action Bool -> IO Graph
filterP g@(AV.Series sr) func = parallel g func >>= \bool -> return . AV.Series . snd . V.unzip $ V.filter (\(b, _) -> b) (V.zip bool sr) 
filterP (AV.NodeList (q, nvc)) func = NV.filterP nvc (\n -> func (AV.Node n)) >>= \f -> return $ AV.NodeList (q, f)
filterP _ _ = return AV.attrValueNan

filterPN :: Int -> Graph -> Action Bool -> IO Graph
filterPN num g@(AV.Series sr) func = parallelN num g func >>= \bool -> return . AV.Series . snd . V.unzip $ V.filter (\(b, _) -> b) (V.zip bool sr) 
filterPN num (AV.NodeList (q, nvc)) func = NV.filterPN num nvc (\n -> func (AV.Node n)) >>= \f -> return $ AV.NodeList (q, f)
filterPN _ _ _ = return AV.attrValueNan

reduce :: (Graph -> Graph -> Graph) -> Graph -> AK.AttrName -> IO (Maybe Graph)
reduce func (AV.NodeList (_, nvc)) attr = NV.reduce func nvc attr
reduce _ _ _ = return Nothing

sum :: Graph -> AK.AttrName -> IO (Maybe Graph)
sum (AV.NodeList (_, nvc)) attr = NV.sum nvc attr
sum _ _ = return Nothing

prod :: Graph -> AK.AttrName -> IO (Maybe Graph)
prod (AV.NodeList (_, nvc)) attr = NV.prod nvc attr
prod _ _ = return Nothing

min :: Graph -> AK.AttrName -> IO (Maybe Graph)
min (AV.NodeList (_, nvc)) attr = NV.min nvc attr
min _ _ = return Nothing

max :: Graph -> AK.AttrName -> IO (Maybe Graph)
max (AV.NodeList (_, nvc)) attr = NV.max nvc attr
max _ _ = return Nothing

mean :: Graph -> AK.AttrName -> IO (Maybe Graph)
mean (AV.NodeList (_, nvc)) attr = NV.mean nvc attr
mean _ _ = return Nothing

cumReduce :: (Graph -> Graph -> Graph) -> Graph -> AK.AttrName -> IO (V.Vector (Maybe Graph))
cumReduce func (AV.NodeList (_, nvc)) attr = NV.cumReduce func nvc attr
cumReduce _ _ _ = return V.empty

cumSum :: Graph -> AK.AttrName -> IO (V.Vector (Maybe Graph))
cumSum (AV.NodeList (_, nvc)) attr = NV.cumSum nvc attr
cumSum _ _ = return V.empty

cumProd :: Graph -> AK.AttrName -> IO (V.Vector (Maybe Graph))
cumProd (AV.NodeList (_, nvc)) attr = NV.cumProd nvc attr
cumProd _ _ = return V.empty

cumMin :: Graph -> AK.AttrName -> IO (V.Vector (Maybe Graph))
cumMin (AV.NodeList (_, nvc)) attr = NV.cumMin nvc attr
cumMin _ _ = return V.empty

cumMax :: Graph -> AK.AttrName -> IO (V.Vector (Maybe Graph))
cumMax (AV.NodeList (_, nvc)) attr = NV.cumMax nvc attr
cumMax _ _ = return V.empty

{-# INLINABLE rollingCore #-}
rollingCore :: Graph -> Int -> AK.AttrName -> (NV.NodeVector Graph -> Int -> V.Vector (NV.NodeVector Graph)) -> IO ()
rollingCore (AV.NodeList (_, nvc)) window attr func = do
  let l = V.length nvc
  let idx = V.fromList [0..(l-1)]
  let rollingV = func nvc window
  nl <- NV.newN l
  NV.setV nvc attr idx $ V.zipWith (\q n -> AV.NodeList (q, n)) nl rollingV
rollingCore _ _ _ _ = return ()

{-# INLINABLE rolling #-}
rolling :: Graph -> Int -> IO ()
rolling g window = rollingCore g window "rolling" NV.rolling

{-# INLINABLE rollingCenter #-}
rollingCenter :: Graph -> Int -> IO ()
rollingCenter g window = rollingCore g window "rolling" NV.rollingCenter

{-# INLINABLE sort #-}
sort :: Graph -> AK.AttrName -> Bool -> IO Graph
sort (AV.NodeList (q, nvc)) attr ascending = NV.sort nvc attr ascending >>= \f -> return $ AV.NodeList (q, f)
sort _ _ _ = return AV.attrValueNan

{-# INLINABLE sortBy #-}
sortBy :: (Ord b) => Graph -> Action b -> Bool -> IO Graph
sortBy (AV.NodeList (q, nvc)) func ascending = NV.sortBy nvc (\n -> func $ AV.Node n) ascending >>= \f -> return $ AV.NodeList (q, f)
sortBy (AV.Series sr) func ascending = V.mapM func sr >>= \v -> return . AV.Series $ VAG.argSortBy v (\i -> sr V.! i) ascending
sortBy _ _ _ = return AV.attrValueNan

{-# INLINABLE sortPN #-}
sortPN :: Int -> Graph -> AK.AttrName -> Bool -> IO Graph
sortPN num (AV.NodeList (q, nvc)) attr ascending = NV.sortPN num nvc attr ascending >>= \f -> return $ AV.NodeList (q, f)
sortPN _ _ _ _ = return AV.attrValueNan

{-# INLINABLE sortS #-}
sortS :: Graph -> [AK.AttrName] -> Bool -> IO Graph
sortS (AV.NodeList (q, nvc)) attrL ascending = NV.sortS nvc attrL ascending >>= \f -> return $ AV.NodeList (q, f)
sortS _ _ _ = return AV.attrValueNan

{-# INLINABLE sortUBy #-}
sortUBy :: forall b. (Ord b, VU.Unbox b) => Graph -> (Graph -> IO b) -> Bool -> IO Graph
sortUBy (AV.NodeList (q, nvc)) func ascending = NV.sortUBy nvc (\n -> func $ AV.Node n) ascending >>= \f -> return $ AV.NodeList (q, f)
sortUBy (AV.Series sr) func ascending = V.mapM func sr >>= \v -> return . AV.Series $ V.backpermute sr $ V.convert $ VAG.argSort (V.convert v :: VU.Vector b) ascending
sortUBy _ _ _ = return AV.attrValueNan

{-# INLINABLE group #-}
group :: Graph -> AK.AttrName -> IO Graph
group (AV.NodeList (_, nvc)) attr = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.group nvc attr) >>= return . AV.Series
group _ _ = return AV.attrValueNan

{-# INLINABLE groupBy #-}
groupBy :: Ord b => Graph -> Action b -> IO Graph
groupBy (AV.NodeList (_, nvc)) func = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupBy nvc (\i -> func $ AV.Node i)) >>= return . AV.Series
groupBy (AV.Series sr) func = V.mapM func sr >>= \v -> do
  let idx = VAG.argSort v True
  let ss = V.backpermute sr idx
  let vs = V.backpermute v idx
  let vg = VAG.group vs
  let lens = V.map V.length vg
  let offsets = V.prescanl' (+) 0 lens
  return . AV.Series $ V.zipWith (\off l -> AV.Series $ V.slice off l ss) offsets lens
groupBy _ _ = return AV.attrValueNan

{-# INLINABLE groupUBy #-}
groupUBy :: forall b. (Ord b, VU.Unbox b) => Graph -> Action b -> IO Graph
groupUBy (AV.NodeList (_, nvc)) func = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupUBy nvc (\i -> func $ AV.Node i)) >>= return . AV.Series
groupUBy (AV.Series sr) func = V.mapM func sr >>= \v -> do
  let idx = V.convert $ VAG.argSort (V.convert v :: VU.Vector b) True
  let ss = V.backpermute sr idx
  let vs = V.backpermute v idx
  let vg = VAG.group vs
  let lens = V.map V.length vg
  let offsets = V.prescanl' (+) 0 lens
  return . AV.Series $ V.zipWith (\off l -> AV.Series $ V.slice off l ss) offsets lens
groupUBy _ _ = return AV.attrValueNan

{-# INLINABLE groupPN #-}
groupPN :: Int -> Graph -> AK.AttrName -> IO Graph
groupPN num (AV.NodeList (_, nvc)) attr = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupPN num nvc attr) >>= return . AV.Series
groupPN _ _ _ = return AV.attrValueNan

{-# INLINABLE groupS #-}
groupS :: Graph -> [AK.AttrName] -> IO Graph
groupS (AV.NodeList (_, nvc)) attrL = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupS nvc attrL) >>= return . AV.Series
groupS _ _ = return AV.attrValueNan

{-# INLINABLE unique #-}
unique :: Graph -> Graph
unique (AV.Series sr) = AV.Series $ V.map AV.Element . V.filter (/= AV.elementValueNan) . V.uniq . VAG.sortAsc $ V.map func sr
  where 
    func (AV.Element v) = v
    func _ = AV.elementValueNan
unique _ = AV.attrValueNan

join :: Graph -> Graph -> AK.AttrName -> Relation Bool -> IO ()
join gA gB attrA func = for_ gA $ \ia -> RiskQuantLib.Graph.filter gB (func ia) >>= ia .< attrA

joinPN :: Int -> Graph -> Graph -> AK.AttrName -> Relation Bool -> IO ()
joinPN num gA gB attr func = parallelN_ num gA $ \ia -> RiskQuantLib.Graph.filter gB (func ia) >>= ia .< attr

connect :: Graph -> Graph -> AK.AttrName -> AK.AttrName -> Relation Bool -> Relation Bool -> IO ()
connect gA gB attrA attrB funcA funcB = ANC.concurrently_ joinLT joinRT
  where
    joinLT = join gA gB attrA funcA
    joinRT = join gB gA attrB $ flip funcB

connectPN :: Int -> Graph -> Graph -> AK.AttrName -> AK.AttrName -> Relation Bool -> Relation Bool -> IO ()
connectPN num gA gB attrA attrB funcA funcB = ANC.concurrently_ joinLT joinRT
  where
    joinLT = joinPN num gA gB attrA funcA
    joinRT = joinPN num gB gA attrB $ flip funcB

fillNan :: Graph -> Graph -> Graph
fillNan (AV.Series sr) v = AV.Series $ V.map (\i -> if AV.isNan i then v else i) sr
fillNan _ _ = AV.attrValueNan

dropNan :: Graph -> Graph
dropNan (AV.Series sr) = AV.Series $ V.filter AV.isNan sr
dropNan _ = AV.attrValueNan

isNan :: Graph -> Graph
isNan (AV.Series sr) = AV.Series $ V.map (\i -> AV.fromBool . AV.isNan $ i) sr
isNan _ = AV.attrValueNan

notNan :: Graph -> Graph
notNan (AV.Series sr) = AV.Series $ V.map (\i -> AV.fromBool . AV.notNan $ i) sr
notNan _ = AV.attrValueNan

whereG :: Graph -> Graph -> Graph -> Graph
whereG (AV.Series sr) t f = AV.Series $ V.imap func sr
  where
    func i (AV.Element (AV.ElemBool j)) = if j then t .! i else f .! i
    func _ _ = AV.attrValueNan
whereG _ _ _ = AV.attrValueNan

select :: Graph -> Graph -> Graph
select (AV.Series sr) v = dropNan . AV.Series $ V.imap func sr
  where
    func i (AV.Element (AV.ElemBool j)) = if j then v .! i else AV.attrValueNan
    func _ _ = AV.attrValueNan
select _ _ = AV.attrValueNan

zipWith :: Relation a -> Graph -> Graph -> IO (V.Vector a)
zipWith func (AV.Series srA) (AV.Series srB) = V.zipWithM func srA srB
zipWith _ _ _ = return V.empty

map :: (Graph -> Graph) -> Graph -> Graph
map func (AV.Series sr) = AV.Series $ V.map func sr
map _ _ = AV.attrValueNan

mapAt :: AK.AttrName -> (Graph -> Graph) -> Graph -> IO ()
mapAt attr func (AV.NodeList (_, nvc)) = do
  NV.for_ nvc $ \n -> do
    old <- n N..> attr
    case old of
      Just v -> n N..< attr $ func v
      Nothing -> return ()
mapAt _ _ _ = return ()

mapAtS :: [AK.AttrName] -> (Graph -> Graph) -> Graph -> IO ()
mapAtS attrL func g = ANC.mapConcurrently_ (\attr -> mapAt attr func g) attrL

toDict :: AK.AttrName -> AK.AttrName -> Graph -> IO AV.ElementDict
toDict attrKey attrValue (AV.NodeList (_, nvc)) = do
  m <- (H.newSized $ V.length nvc) :: IO AV.ElementDict
  V.forM_ nvc $ \n -> do
    key <- (N..>) n attrKey
    case key of
      Just (AV.Element e) -> (N..>) n attrValue >>= \value -> case value of
        Just (AV.Element k) -> H.insert m e k
        _ -> H.insert m e AV.elementValueNan
      _ -> return ()
  return m
toDict _ _ _ = H.new

lookup :: Graph -> AV.ElementDict -> IO Graph
lookup (AV.Element e) dict = H.lookup dict e >>= \v -> case v of 
  Just vv -> return $ AV.Element vv
  Nothing -> return AV.attrValueNan
lookup _ _ = return AV.attrValueNan 

insert :: Graph -> Graph -> AV.ElementDict -> IO ()
insert (AV.Element e) (AV.Element v) dict = H.insert dict e v
insert _ _ _ = return ()
