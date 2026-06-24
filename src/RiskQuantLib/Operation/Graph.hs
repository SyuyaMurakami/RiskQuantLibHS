{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Operation.Graph (
  Graph,
  Action,
  Relation,
  new,
  newN
) where

import qualified RiskQuantLib.Attribute.Value as AV
import qualified RiskQuantLib.Node.Node as N
import qualified RiskQuantLib.Node.NodeVector as NV

type Graph = AV.AttrValue
type Action a = Graph -> IO a
type Relation a = Graph -> Graph -> IO a

new :: IO Graph
new = ((, NV.new) <$> N.new) >>= return . AV.NodeList

newN :: Int -> IO Graph
newN num = ((,) <$> N.new <*> (NV.newN num)) >>= return . AV.NodeList
