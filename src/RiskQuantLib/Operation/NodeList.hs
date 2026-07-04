module RiskQuantLib.Operation.NodeList (
  get,
  has,
  set,
  setAt,
  setBy,
  getPN,
  getS,
  hasPN,
  reduce,
  RiskQuantLib.Operation.NodeList.sum,
  prod,
  RiskQuantLib.Operation.NodeList.min,
  RiskQuantLib.Operation.NodeList.max,
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
  sortPN,
  sortS,
  group,
  groupPN,
  groupS,
  groupKey,
  dropDuplicateAt,
  dropDuplicateAtS,
  mapAt,
  mapAtS,
  mapAtSPN,
  toDict
) where

import qualified RiskQuantLib.Operation.Graph as OG
import qualified RiskQuantLib.Operation.Dictionary as OD
import qualified RiskQuantLib.Operation.Vector as OPV
import qualified RiskQuantLib.Attribute.Key as AK
import qualified RiskQuantLib.Attribute.Value as AV
import qualified RiskQuantLib.Node.Node as N
import qualified RiskQuantLib.Node.NodeVector as NV

import qualified Data.Vector.Strict as V
import qualified Data.HashTable.IO as H
import qualified Data.Maybe as MB
import qualified Control.Concurrent.Async as ANC

get :: OG.Graph -> AK.AttrName -> IO OG.Graph
get (AV.NodeList (_, nvc)) attr = NV.get nvc attr AV.attrValueNan >>= return . AV.Series
get _ _ = return AV.attrValueNan

has :: OG.Graph -> AK.AttrName -> IO OG.Graph
has (AV.NodeList (q, nvc)) attr = NV.has nvc attr >>= \vb -> return $ AV.NodeList (q, vb)
has _ _ = OG.new

set :: OG.Graph -> AK.AttrName -> OG.Graph -> IO ()
set (AV.NodeList (_, nvc)) attr (AV.Series sr) = NV.set nvc attr [0..(V.length sr - 1)] (V.toList sr)
set (AV.NodeList (_, nvcA)) attr (AV.NodeList (_, nvcB)) = NV.set nvcA attr [0..(V.length nvcB - 1)] (V.toList $ V.map AV.Node nvcB)
set _ _ _ = return ()

setAt :: OG.Graph -> AK.AttrName -> [NV.NodeIndex] -> [OG.Graph] -> IO ()
setAt (AV.NodeList (_, nvc)) attr key value = NV.set nvc attr key value
setAt _ _ _ _ = return ()

setBy :: OG.Graph -> AK.AttrName -> AK.AttrName -> OD.ElementDict -> IO ()
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

getPN :: Int -> OG.Graph -> AK.AttrName -> IO OG.Graph
getPN num (AV.NodeList (_, nvc)) attr = NV.getPN num nvc attr AV.attrValueNan >>= return . AV.Series
getPN _ _ _ = return AV.attrValueNan

getS :: OG.Graph -> [AK.AttrName] -> IO OG.Graph
getS (AV.NodeList (_, nvc)) attrL = NV.getS nvc attrL AV.attrValueNan >>= \vl -> return . AV.Series $ V.map AV.Series vl
getS _ _ = return AV.attrValueNan

hasPN :: Int -> OG.Graph -> AK.AttrName -> IO OG.Graph
hasPN num (AV.NodeList (q, nvc)) attr = NV.hasPN num nvc attr >>= \vb -> return $ AV.NodeList (q, vb)
hasPN _ _ _ = OG.new

reduce :: (OG.Graph -> OG.Graph -> OG.Graph) -> OG.Graph -> AK.AttrName -> IO OG.Graph
reduce func (AV.NodeList (_, nvc)) attr = NV.reduce func nvc attr >>= return . MB.fromMaybe AV.attrValueNan
reduce _ _ _ = return AV.attrValueNan

sum :: OG.Graph -> AK.AttrName -> IO OG.Graph
sum g attr = reduce (+) g attr

prod :: OG.Graph -> AK.AttrName -> IO OG.Graph
prod g attr = reduce (*) g attr

min :: OG.Graph -> AK.AttrName -> IO OG.Graph
min g attr = reduce Prelude.min g attr

max :: OG.Graph -> AK.AttrName -> IO OG.Graph
max g attr = reduce Prelude.max g attr

mean :: OG.Graph -> AK.AttrName -> IO OG.Graph
mean (AV.NodeList (_, nvc)) attr = NV.mean nvc attr >>= return . MB.fromMaybe AV.attrValueNan
mean _ _ = return AV.attrValueNan

cumReduce :: (OG.Graph -> OG.Graph -> OG.Graph) -> OG.Graph -> AK.AttrName -> IO OG.Graph
cumReduce func (AV.NodeList (_, nvc)) attr = NV.cumReduce func nvc attr >>= return . AV.Series . V.map (MB.fromMaybe AV.attrValueNan)
cumReduce _ _ _ = return AV.attrValueNan

cumSum :: OG.Graph -> AK.AttrName -> IO OG.Graph
cumSum g attr = cumReduce (+) g attr

cumProd :: OG.Graph -> AK.AttrName -> IO OG.Graph
cumProd g attr = cumReduce (*) g attr

cumMin :: OG.Graph -> AK.AttrName -> IO OG.Graph
cumMin g attr = cumReduce Prelude.min g attr

cumMax :: OG.Graph -> AK.AttrName -> IO OG.Graph
cumMax g attr = cumReduce Prelude.max g attr

{-# INLINABLE rollingCore #-}
rollingCore :: OG.Graph -> Int -> AK.AttrName -> (NV.NodeVector OG.Graph -> Int -> V.Vector (NV.NodeVector OG.Graph)) -> IO ()
rollingCore (AV.NodeList (_, nvc)) window attr func = do
  let l = V.length nvc
  let idx = V.fromList [0..(l-1)]
  let rollingV = func nvc window
  nl <- NV.newN l
  NV.setV nvc attr idx $ V.zipWith (\q n -> AV.NodeList (q, n)) nl rollingV
rollingCore _ _ _ _ = return ()

{-# INLINABLE rolling #-}
rolling :: OG.Graph -> Int -> IO ()
rolling g window = rollingCore g window "rolling" NV.rolling

{-# INLINABLE rollingCenter #-}
rollingCenter :: OG.Graph -> Int -> IO ()
rollingCenter g window = rollingCore g window "rolling" NV.rollingCenter

{-# INLINABLE sort #-}
sort :: OG.Graph -> AK.AttrName -> Bool -> IO OG.Graph
sort (AV.NodeList (q, nvc)) attr ascending = NV.sort nvc attr ascending >>= \f -> return $ AV.NodeList (q, f)
sort _ _ _ = return AV.attrValueNan

{-# INLINABLE sortPN #-}
sortPN :: Int -> OG.Graph -> AK.AttrName -> Bool -> IO OG.Graph
sortPN num (AV.NodeList (q, nvc)) attr ascending = NV.sortPN num nvc attr ascending >>= \f -> return $ AV.NodeList (q, f)
sortPN _ _ _ _ = return AV.attrValueNan

{-# INLINABLE sortS #-}
sortS :: OG.Graph -> [AK.AttrName] -> Bool -> IO OG.Graph
sortS (AV.NodeList (q, nvc)) attrL ascending = NV.sortS nvc attrL ascending >>= \f -> return $ AV.NodeList (q, f)
sortS _ _ _ = return AV.attrValueNan

{-# INLINABLE group #-}
group :: OG.Graph -> AK.AttrName -> IO OG.Graph
group (AV.NodeList (_, nvc)) attr = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.group nvc attr) >>= return . AV.Series
group _ _ = return AV.attrValueNan

{-# INLINABLE groupPN #-}
groupPN :: Int -> OG.Graph -> AK.AttrName -> IO OG.Graph
groupPN num (AV.NodeList (_, nvc)) attr = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupPN num nvc attr) >>= return . AV.Series
groupPN _ _ _ = return AV.attrValueNan

{-# INLINABLE groupS #-}
groupS :: OG.Graph -> [AK.AttrName] -> IO OG.Graph
groupS (AV.NodeList (_, nvc)) attrL = (V.zipWith (\n g -> AV.NodeList (n, g)) <$> NV.newN (V.length nvc) <*> NV.groupS nvc attrL) >>= return . AV.Series
groupS _ _ = return AV.attrValueNan

groupKey :: OG.Graph -> AK.AttrName -> IO OG.Graph
groupKey (AV.Series sr) attr = V.mapM func sr >>= return . AV.Series
  where
    func (AV.NodeList (_, nvc))
      | V.length nvc == 0 = return AV.attrValueNan
      | otherwise = V.head nvc N...> attr
    func _ = return AV.attrValueNan
groupKey _ _ = return AV.attrValueNan

dropDuplicateAt :: OG.Graph -> AK.AttrName -> IO OG.Graph
dropDuplicateAt (AV.NodeList (n, nvc)) attr = do
  nvcUnique <- NV.dropDuplicateByAt nvc AV.is attr
  return $ AV.NodeList (n, nvcUnique)
dropDuplicateAt _ _ = return AV.attrValueNan

dropDuplicateAtS :: OG.Graph -> [AK.AttrName] -> IO OG.Graph
dropDuplicateAtS (AV.NodeList (n, nvc)) attrL = do
  nvcUnique <- NV.dropDuplicateByAtS nvc AV.is attrL
  return $ AV.NodeList (n, nvcUnique)
dropDuplicateAtS _ _ = return AV.attrValueNan

mapAt :: AK.AttrName -> (OG.Graph -> OG.Graph) -> OG.Graph -> IO ()
mapAt attr func (AV.NodeList (_, nvc)) = do
  NV.for_ nvc $ \n -> do
    old <- n N..> attr
    case old of
      Just v -> n N..< attr $ func v
      Nothing -> return ()
mapAt _ _ _ = return ()

mapAtS :: [AK.AttrName] -> (OG.Graph -> OG.Graph) -> OG.Graph -> IO ()
mapAtS attrL func g = ANC.mapConcurrently_ (\attr -> mapAt attr func g) attrL

mapAtSPN :: Int -> [AK.AttrName] -> (OG.Graph -> OG.Graph) -> OG.Graph -> IO ()
mapAtSPN num attrL func (AV.NodeList (n, nvc)) = flip ANC.mapConcurrently_ (OPV.splitTo nvc num) $ \block -> ANC.mapConcurrently_ (\attr -> mapAt attr func $ AV.NodeList (n, block)) attrL
mapAtSPN _ _ _ _ = return ()

toDict :: AK.AttrName -> AK.AttrName -> OG.Graph -> IO OD.ElementDict
toDict attrKey attrValue (AV.NodeList (_, nvc)) = do
  m <- (H.newSized $ V.length nvc) :: IO OD.ElementDict
  V.forM_ nvc $ \n -> do
    key <- (N..>) n attrKey
    case key of
      Just (AV.Element e) -> (N..>) n attrValue >>= \value -> case value of
        Just (AV.Element k) -> H.insert m e k
        _ -> H.insert m e AV.elementValueNan
      _ -> return ()
  return m
toDict _ _ _ = H.new
