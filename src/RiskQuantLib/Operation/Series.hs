{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Operation.Series (
  unique,
  fillNan,
  ffillNan,
  bfillNan,
  dropNan,
  isNan,
  notNan,
  shift,
  whereG,
  select,
  RiskQuantLib.Operation.Series.zipWith,
  RiskQuantLib.Operation.Series.zipWithM,
  RiskQuantLib.Operation.Series.map,
  RiskQuantLib.Operation.Series.mapM,
  apply,
  applyM,
  reduce,
  RiskQuantLib.Operation.Series.sum,
  prod,
  RiskQuantLib.Operation.Series.min,
  RiskQuantLib.Operation.Series.max,
  minG,
  maxG,
  mean,
  std,
  cumReduce,
  cumSum,
  cumProd,
  cumMin,
  cumMax
) where

import qualified RiskQuantLib.Operation.Vector as OPV
import qualified RiskQuantLib.Operation.Graph as OG
import qualified RiskQuantLib.Operation.Mix as OM
import qualified RiskQuantLib.Attribute.Value as AV

import qualified Data.Vector.Strict as V
import qualified Data.Vector.Unboxed as VU

{-# INLINABLE unique #-}
unique :: OG.Graph -> OG.Graph
unique (AV.Series sr) = AV.Series $ V.map AV.Element . V.filter (/= AV.elementValueNan) . V.uniq . OPV.sortAsc $ V.map func sr
  where 
    func (AV.Element v) = v
    func _ = AV.elementValueNan
unique _ = AV.attrValueNan

fillNan :: OG.Graph -> OG.Graph -> OG.Graph
fillNan (AV.Series sr) v = AV.Series $ V.map (\i -> if AV.isNan i then v else i) sr
fillNan _ _ = AV.attrValueNan

ffillNan :: OG.Graph -> OG.Graph
ffillNan (AV.Series sr) = AV.Series $ V.postscanl' (\accu i -> if AV.isNan i then accu else i) AV.attrValueNan sr
ffillNan _ = AV.attrValueNan

bfillNan :: OG.Graph -> OG.Graph
bfillNan (AV.Series sr) = AV.Series $ V.postscanr' (\i accu -> if AV.isNan i then accu else i) AV.attrValueNan sr
bfillNan _ = AV.attrValueNan

dropNan :: OG.Graph -> OG.Graph
dropNan (AV.Series sr) = AV.Series $ V.filter AV.notNan sr
dropNan _ = AV.attrValueNan

isNan :: OG.Graph -> OG.Graph
isNan (AV.Series sr) = AV.Series $ V.map (\i -> AV.fromBool . AV.isNan $ i) sr
isNan _ = AV.attrValueNan

notNan :: OG.Graph -> OG.Graph
notNan (AV.Series sr) = AV.Series $ V.map (\i -> AV.fromBool . AV.notNan $ i) sr
notNan _ = AV.attrValueNan

shift :: Int -> OG.Graph -> OG.Graph
shift n g@(AV.Series sr)
  | n == 0 = g
  | n > 0 && n < l = AV.Series $ (V.replicate n AV.attrValueNan) V.++ (V.take (l - n) sr)
  | n < 0 && n > (-1) * l = AV.Series $ (V.drop nAbs sr) V.++ (V.replicate nAbs AV.attrValueNan)
  | otherwise = AV.Series $ V.replicate l AV.attrValueNan
  where
    l = V.length sr
    nAbs = (-1) * n
shift _ _ = AV.attrValueNan

whereG :: OG.Graph -> OG.Graph -> OG.Graph -> OG.Graph
whereG (AV.Series sr) t f = AV.Series $ V.imap func sr
  where
    func i (AV.Element (AV.ElemBool j)) = if j then t OM..! i else f OM..! i
    func _ _ = AV.attrValueNan
whereG _ _ _ = AV.attrValueNan

select :: OG.Graph -> OG.Graph -> OG.Graph
select (AV.Series sr) v = dropNan . AV.Series $ V.imap func sr
  where
    func i (AV.Element (AV.ElemBool j)) = if j then v OM..! i else AV.attrValueNan
    func _ _ = AV.attrValueNan
select _ _ = AV.attrValueNan

zipWith :: (OG.Graph -> OG.Graph -> a) -> OG.Graph -> OG.Graph -> V.Vector a
zipWith func (AV.Series srA) (AV.Series srB) = V.zipWith func srA srB
zipWith _ _ _ = V.empty

zipWithM :: OG.Relation a -> OG.Graph -> OG.Graph -> IO (V.Vector a)
zipWithM func (AV.Series srA) (AV.Series srB) = V.zipWithM func srA srB
zipWithM _ _ _ = return V.empty

map :: (OG.Graph -> a) -> OG.Graph -> V.Vector a
map func (AV.Series sr) = V.map func sr
map _ _ = V.empty

mapM :: OG.Action a -> OG.Graph -> IO (V.Vector a)
mapM func (AV.Series sr) = V.mapM func sr
mapM _ _ = return V.empty

apply :: (OG.Graph -> OG.Graph) -> OG.Graph -> OG.Graph
apply func g = AV.Series $ RiskQuantLib.Operation.Series.map func g

applyM :: (OG.Graph -> IO OG.Graph) -> OG.Graph -> IO OG.Graph
applyM func g = RiskQuantLib.Operation.Series.mapM func g >>= return . AV.Series

stepBy :: (OG.Graph -> OG.Graph -> OG.Graph) -> OG.Graph -> OG.Graph -> OG.Graph
stepBy func accu i
  | AV.isNan i = accu
  | otherwise = if AV.isNan accu then i else func accu i

reduce :: (OG.Graph -> OG.Graph -> OG.Graph) -> OG.Graph -> OG.Graph
reduce func (AV.Series sr) = V.foldl' (stepBy func) AV.attrValueNan sr
reduce _ _ = AV.attrValueNan

minG :: OG.Graph -> OG.Graph
minG = reduce Prelude.min

maxG :: OG.Graph -> OG.Graph
maxG = reduce Prelude.max

{-# INLINABLE applyDouble #-}
applyDouble :: (VU.Vector Double -> Maybe Double) -> OG.Graph -> OG.Graph
applyDouble func (AV.Series sr) = case res of
    Nothing -> AV.attrValueNan
    Just r -> AV.fromDouble r
  where
    av = V.map AV.toDouble . V.filter AV.isDouble $ sr 
    res = func (V.convert av :: VU.Vector Double)
applyDouble _ _ = AV.attrValueNan

{-# INLINABLE sum #-}
sum :: OG.Graph -> OG.Graph
sum = applyDouble $ OPV.reduce (const False) (+)

{-# INLINABLE prod #-}
prod :: OG.Graph -> OG.Graph
prod = applyDouble $ OPV.reduce (const False) (*)

{-# INLINABLE min #-}
min :: OG.Graph -> OG.Graph
min = applyDouble $ OPV.reduce (const False) Prelude.min

{-# INLINABLE max #-}
max :: OG.Graph -> OG.Graph
max = applyDouble $ OPV.reduce (const False) Prelude.max

{-# INLINABLE mean #-}
mean :: OG.Graph -> OG.Graph
mean = applyDouble $ OPV.mean (const False)

{-# INLINABLE std #-}
std :: OG.Graph -> OG.Graph
std = applyDouble $ OPV.std (const False)

cumReduce :: (OG.Graph -> OG.Graph -> OG.Graph) -> OG.Graph -> OG.Graph
cumReduce func (AV.Series sr) = AV.Series $ V.postscanl' (stepBy func) AV.attrValueNan sr
cumReduce _ _ = AV.attrValueNan

cumSum :: OG.Graph -> OG.Graph
cumSum g = cumReduce (+) g

cumProd :: OG.Graph -> OG.Graph
cumProd g = cumReduce (*) g

cumMin :: OG.Graph -> OG.Graph
cumMin g = cumReduce Prelude.min g

cumMax :: OG.Graph -> OG.Graph
cumMax g = cumReduce Prelude.max g
