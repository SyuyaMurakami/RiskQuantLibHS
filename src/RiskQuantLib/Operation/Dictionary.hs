module RiskQuantLib.Operation.Dictionary (
  ElementDict,
  makeDict,
  RiskQuantLib.Operation.Dictionary.lookup,
  insert
) where

import qualified RiskQuantLib.Operation.Graph as OG
import qualified RiskQuantLib.Attribute.Value as AV

import qualified Data.HashTable.IO as H
import qualified Data.Vector.Strict as V

type ElementDict = H.LinearHashTable AV.ElementValue AV.ElementValue

makeDict :: [AV.ElementValue] -> [AV.ElementValue] -> IO ElementDict
makeDict key value = H.new >>= \m -> V.zipWithM_ (\k v -> H.insert m k v) (V.fromList key) (V.fromList value) >> return m

lookup :: OG.Graph -> ElementDict -> IO OG.Graph
lookup (AV.Element e) dict = H.lookup dict e >>= \v -> case v of 
  Just vv -> return $ AV.Element vv
  Nothing -> return AV.attrValueNan
lookup _ _ = return AV.attrValueNan 

insert :: OG.Graph -> OG.Graph -> ElementDict -> IO ()
insert (AV.Element e) (AV.Element v) dict = H.insert dict e v
insert _ _ _ = return ()
