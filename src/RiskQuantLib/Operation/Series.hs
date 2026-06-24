{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Operation.Series (
  unique,
  fillNan,
  dropNan,
  isNan,
  notNan,
  whereG,
  select,
  RiskQuantLib.Operation.Series.zipWith,
  RiskQuantLib.Operation.Series.map,
) where

import qualified RiskQuantLib.Operation.Vector as OPV
import qualified RiskQuantLib.Operation.Graph as OG
import qualified RiskQuantLib.Operation.Mix as OM
import qualified RiskQuantLib.Attribute.Value as AV

import qualified Data.Vector.Strict as V

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

dropNan :: OG.Graph -> OG.Graph
dropNan (AV.Series sr) = AV.Series $ V.filter AV.isNan sr
dropNan _ = AV.attrValueNan

isNan :: OG.Graph -> OG.Graph
isNan (AV.Series sr) = AV.Series $ V.map (\i -> AV.fromBool . AV.isNan $ i) sr
isNan _ = AV.attrValueNan

notNan :: OG.Graph -> OG.Graph
notNan (AV.Series sr) = AV.Series $ V.map (\i -> AV.fromBool . AV.notNan $ i) sr
notNan _ = AV.attrValueNan

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

zipWith :: OG.Relation a -> OG.Graph -> OG.Graph -> IO (V.Vector a)
zipWith func (AV.Series srA) (AV.Series srB) = V.zipWithM func srA srB
zipWith _ _ _ = return V.empty

map :: (OG.Graph -> OG.Graph) -> OG.Graph -> OG.Graph
map func (AV.Series sr) = AV.Series $ V.map func sr
map _ _ = AV.attrValueNan
