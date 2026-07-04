{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module RiskQuantLib.Operation.Vector (
  iLocN,
  fillNan,
  dropNan,
  isNan,
  notNan,
  whereV,
  select,
  rolling,
  rollingCenter,
  splitTo,
  splitBy,
  sortAsc,
  sortDesc,
  sort,
  argSortBy,
  argSort,
  compareS,
  equalS,
  argSortByS,
  argSortS,
  sortS,
  group,
  groupBy,
  uniqBy,
  argUniqBy,
  dropDuplicateBy,
  dropDuplicate,
  mean,
  std,
  reduce, 
  cumReduce
) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Quicksort as VQS
import qualified Data.Vector.Algorithms.Intro as Intro
import Control.Monad.ST (runST)
import Data.Ord (Down(..))
import Data.Maybe (fromMaybe)

{-# INLINABLE iLocN #-}
iLocN :: forall a v. (VG.Vector v a) => v a -> Int -> a
iLocN x i = x VG.! (if i < 0 then VG.length x + i else i)

{-# INLINABLE fillNan #-}
fillNan :: forall a v. (VG.Vector v a, VG.Vector v (Maybe a)) => v (Maybe a) -> a -> v a
fillNan x value = VG.map (fromMaybe value) x 

{-# INLINABLE dropNan #-}
dropNan :: forall a v. (VG.Vector v a, VG.Vector v (Maybe a)) => v (Maybe a) -> v a
dropNan x = VG.mapMaybe id x

{-# INLINABLE isNan #-}
isNan :: forall a v. (VG.Vector v Bool, VG.Vector v (Maybe a)) => v (Maybe a) -> v Bool
isNan x = flip VG.map x $ \i -> case i of
  Nothing -> True
  Just _ -> False

{-# INLINABLE notNan #-}
notNan :: forall a v. (VG.Vector v Bool, VG.Vector v (Maybe a)) => v (Maybe a) -> v Bool
notNan x = flip VG.map x $ \i -> case i of
  Nothing -> False
  Just _ -> True 

{-# INLINABLE whereV #-}
whereV :: forall a v. (VG.Vector v a, VG.Vector v Bool) => v Bool -> v a -> v a -> v a
whereV logic xTrue xFalse = VG.zipWith3 (\l t f -> if l then t else f) logic xTrue xFalse

{-# INLINABLE select #-}
select :: forall a v. (VG.Vector v a, VG.Vector v Bool, VG.Vector v (Bool, a)) => v Bool -> v a -> v a
select logic x = snd . VG.unzip $ VG.filter (\(l, _) -> l) $ VG.zip logic x

{-# INLINABLE rolling #-}
rolling :: forall a v. (VG.Vector v a, VG.Vector v (v a)) => v a -> Int -> v (v a)
rolling x window
  | window <= 0 = VG.empty
  | otherwise = VG.generate (VG.length x) $ \i -> 
    let 
      start = max 0 (i - window + 1)
      len = min (i + 1) window
    in VG.slice start len x

{-# INLINABLE rollingCenter #-}
rollingCenter :: forall a v. (VG.Vector v a, VG.Vector v (v a)) => v a -> Int -> v (v a)
rollingCenter x window = let lenT = VG.length x in VG.generate lenT $ \i -> 
  let 
    offset = (window - 1) `div` 2
    start = max 0 (i - offset)
    end = min (lenT - 1) (i + (window - 1 - offset))
    len = end - start + 1
  in VG.slice start len x

{-# INLINABLE splitTo #-}
splitTo :: forall a v. (VG.Vector v a) => v a -> Int -> [v a]
splitTo x num
  | lenT == 0 = []
  | num <= 1 = [x]
  | otherwise = go1 0 0 
  where
    lenT = VG.length x
    n = Prelude.min num lenT
    (len0, r) = lenT `quotRem` n
    len1 = len0 + 1
    go1 !i !start
      | i >= r = go2 i start
      | otherwise = VG.slice start len1 x : go1 (i + 1) (start + len1)
    go2 !i !start
      | i >= n = []
      | otherwise = VG.slice start len0 x : go2 (i + 1) (start + len0)

{-# INLINABLE splitBy #-}
splitBy :: forall a v. (VG.Vector v a) => v a -> Int -> [v a]
splitBy x batch = if VG.length x == 0 then [] else let (s, e) = VG.splitAt batch x in (s:(splitBy e batch))

{-# INLINABLE sortAsc #-}
sortAsc :: forall a v. (Ord a, VG.Vector v a) => v a -> v a
sortAsc x = VQS.sort x

{-# INLINABLE sortDesc #-}
sortDesc :: forall a v. (Ord a, VG.Vector v a, VG.Vector v (Down a)) => v a -> v a
sortDesc x = VG.map (\(Down i) -> i) . sortAsc . VG.map Down $ x

{-# INLINABLE sort #-}
sort :: forall a v. (Ord a, VG.Vector v a, VG.Vector v (Down a)) => v a -> Bool -> v a
sort x ascending = if ascending then sortAsc x else sortDesc x

{-# INLINABLE argSortBy #-}
argSortBy :: forall a b v. (Ord a, VG.Vector v a, VG.Vector v (a, Int), VG.Vector v (Down (a, Int)), VG.Vector v b) => v a -> (Int -> b) -> Bool -> v b
argSortBy x func ascending = let pairs = VG.imap (\idx val -> (val, idx)) x in VG.map (func . snd) $ sort pairs ascending

{-# INLINABLE argSort #-}
argSort :: forall a v. (Ord a, VG.Vector v a, VG.Vector v (a, Int), VG.Vector v (Down (a, Int)), VG.Vector v Int) => v a -> Bool -> v Int
argSort x ascending = argSortBy x id ascending

{-# INLINABLE compareS #-}
compareS :: forall a v. (Ord a, VG.Vector v a) => [v a] -> Bool -> Int -> Int -> Ordering
compareS [] _ _ _ = EQ
compareS (c:cs) ascending idx1 idx2 =
  case logic of
    EQ -> compareS cs ascending idx1 idx2
    other -> other
  where
    v1 = VG.unsafeIndex c idx1
    v2 = VG.unsafeIndex c idx2
    logic = if ascending then compare v1 v2 else compare v2 v1

{-# INLINABLE equalS #-}
equalS :: forall a v. (VG.Vector v a, VG.Vector v Bool, VG.Vector v (a, a), VG.Vector v (v a)) => v (v a) -> (a -> a -> Bool) -> Int -> Int -> Bool
equalS cs func idx1 idx2 = VG.and $ VG.zipWith func v1 v2
  where
    (v1, v2) = VG.unzip $ VG.map (\c -> (VG.unsafeIndex c idx1, VG.unsafeIndex c idx2)) cs

{-# INLINABLE argSortByS #-}
argSortByS :: forall a b v. (Ord a, VG.Vector v a, VG.Vector v Int, VG.Vector v b) => [v a] -> (Int -> b) -> Bool -> v b
argSortByS [] _ _ = VG.empty
argSortByS xs func ascending
  | n == 0 = VG.empty
  | n == 1 = VG.fromList [func 0]
  | otherwise = runST $ do
    indices <- VG.thaw (VG.enumFromN 0 n)
    Intro.sortBy (compareS xs ascending) indices
    frozenIndices <- VG.freeze indices
    return $ VG.map func frozenIndices
  where
    n = minimum $ map VG.length xs

{-# INLINABLE argSortS #-}
argSortS :: forall a v. (Ord a, VG.Vector v a, VG.Vector v Int) => [v a] -> Bool -> v Int
argSortS xs ascending = argSortByS xs id ascending

{-# INLINABLE sortS #-}
sortS :: forall a v. (Ord a, VG.Vector v a, VG.Vector v Int) => [v a] -> Bool -> [v a]
sortS xs ascending = let frozenIndices = argSortS xs ascending in map (\c -> VG.backpermute c frozenIndices) xs

{-# INLINABLE group #-}
group :: forall a v. (Eq a, VG.Vector v a, VG.Vector v (v a)) => v a -> v (v a)
group x = VG.fromList $ VG.group x

{-# INLINABLE groupBy #-}
groupBy :: forall a b v. (Eq b, Ord b, VG.Vector v Int, VG.Vector v (b, Int), VG.Vector v (Down (b, Int)), VG.Vector v a, VG.Vector v b, VG.Vector v (v a), VG.Vector v (v b)) => (a -> b) -> v a -> v (v a)
groupBy func x = VG.zipWith (\s offlen -> VG.slice s offlen xSorted) splitStart accordingGroupLen
  where
    according = VG.map func x
    idx = argSort according True
    accordingSorted = VG.backpermute according idx
    xSorted = VG.backpermute x idx
    accordingGroup = group accordingSorted
    accordingGroupLen = VG.map VG.length accordingGroup
    splitStart = VG.prescanl' (+) 0 accordingGroupLen

{-# INLINABLE uniqByWith #-}
uniqByWith :: forall a b v. (VG.Vector v a, VG.Vector v b) => (a -> a -> Bool) -> ((Int, a) -> b) -> v a -> v b
uniqByWith eq f v = runST $ do
  let !n = VG.length v
  if n == 0
  then pure VG.empty
  else do
    mv <- VGM.unsafeNew n
    let !x0 = VG.unsafeIndex v 0
    VGM.unsafeWrite mv 0 $ f (0, x0)
    let go !i !j !prev
          | i == n = VG.unsafeFreeze (VGM.slice 0 j mv)
          | otherwise =
              let !x = VG.unsafeIndex v i
              in if eq prev x
                then go (i + 1) j prev
                else do
                  VGM.unsafeWrite mv j $ f (i, x)
                  go (i + 1) (j + 1) x
    go 1 1 x0

{-# INLINABLE uniqBy #-}
uniqBy :: forall a v. (VG.Vector v a) => (a -> a -> Bool) -> v a -> v a
uniqBy eq v = uniqByWith eq snd v

{-# INLINABLE argUniqBy #-}
argUniqBy :: forall a v. (VG.Vector v a, VG.Vector v Int) => (a -> a -> Bool) -> v a -> v Int
argUniqBy eq v = uniqByWith eq fst v

{-# INLINABLE dropDuplicateBy #-}
dropDuplicateBy :: forall a v. (Ord a, VG.Vector v a) => (a -> a -> Bool) -> v a -> v a
dropDuplicateBy func x = uniqBy func . sortAsc $ x

{-# INLINABLE dropDuplicate #-}
dropDuplicate :: forall a v. (Ord a, VG.Vector v a) => v a -> v a
dropDuplicate x = VG.uniq . sortAsc $ x

{-# INLINABLE mean #-}
mean :: forall a v. (Fractional a, VG.Vector v a) => (a -> Bool) -> v a -> Maybe a
mean invalid x = if c == 0 then Nothing else Just (s / (fromIntegral c))
  where
    func accu@(count, accuValue) i = if invalid i then accu else (count + 1, accuValue + i)
    (c, s) = VG.foldl' func (0 :: Integer, 0) x

{-# INLINABLE std #-}
std :: forall a v. (Fractional a, Floating a, VG.Vector v a) => (a -> Bool) -> v a -> Maybe a
std invalid = finish . VG.foldl' step (0 :: Integer, 0, 0)
  where
    step (!n, !average, !m2) x
      | invalid x = (n, average, m2)
      | otherwise =
          let n'     = n + 1
              delta  = x - average
              average'  = average + delta / fromIntegral n'
              delta2 = x - average'
              m2'    = m2 + delta * delta2
          in (n', average', m2')
    finish (n, _, m2)
      | n < 2     = Nothing
      | otherwise = Just (sqrt (m2 / fromIntegral (n - 1)))

{-# INLINABLE stepR #-}
stepR :: (a -> Bool) -> (a -> a -> a) -> Maybe a -> a -> Maybe a
stepR invalid f accu i
  | invalid i = accu
  | otherwise = case accu of
      Nothing -> Just i
      Just r -> Just $ f r i

{-# INLINABLE reduce #-}
reduce :: forall a v. (VG.Vector v a) => (a -> Bool) -> (a -> a -> a) -> v a -> Maybe a
reduce invalid func x = VG.foldl' (stepR invalid func) Nothing x

{-# INLINABLE cumReduce #-}
cumReduce :: forall a v. (VG.Vector v a, VG.Vector v (Maybe a)) => (a -> Bool) -> (a -> a -> a) -> v a -> v (Maybe a)
cumReduce invalid func x = VG.postscanl' (stepR invalid func) Nothing x
