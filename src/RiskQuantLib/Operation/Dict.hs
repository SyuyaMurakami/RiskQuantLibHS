{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Operation.Dict (
  RiskQuantLib.Operation.Dict.lookup,
  insert
) where

import qualified RiskQuantLib.Operation.Graph as OG
import qualified RiskQuantLib.Attribute.Value as AV

import qualified Data.HashTable.IO as H

lookup :: OG.Graph -> AV.ElementDict -> IO OG.Graph
lookup (AV.Element e) dict = H.lookup dict e >>= \v -> case v of 
  Just vv -> return $ AV.Element vv
  Nothing -> return AV.attrValueNan
lookup _ _ = return AV.attrValueNan 

insert :: OG.Graph -> OG.Graph -> AV.ElementDict -> IO ()
insert (AV.Element e) (AV.Element v) dict = H.insert dict e v
insert _ _ _ = return ()
