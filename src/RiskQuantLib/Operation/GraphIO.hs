{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Operation.GraphIO (
  columnsAttr,
  readCSV,
  columns,
  mapAtAll,
  headN,
  transpose,
  toCSV
) where

import qualified RiskQuantLib.Attribute.Key as AK
import qualified RiskQuantLib.Attribute.Value as AV
import qualified RiskQuantLib.Tool.File as TF
import qualified RiskQuantLib.Node.Node as N
import qualified RiskQuantLib.Node.NodeVector as NV
import qualified RiskQuantLib.Operation.Graph as OG
import qualified RiskQuantLib.Operation.NodeList as ONL
import qualified RiskQuantLib.Operation.Mix as OM

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector.Strict as V
import qualified Control.Concurrent.Async as ANC

columnsAttr :: AK.AttrName
columnsAttr = "columns"

readCSV :: FilePath -> IO OG.Graph
readCSV path = TF.readFileWithUtf8 path >>= \content -> do
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

columns :: OG.Graph -> IO [AK.AttrName]
columns g@(AV.NodeList (_, _)) = do
  col <- g OM..> columnsAttr
  case col of
    Just c -> OM.for c (return . T.unpack . AV.toText) >>= return . V.toList
    Nothing -> return []
columns _ = return []

mapAtAll :: (OG.Graph -> OG.Graph) -> OG.Graph -> IO ()
mapAtAll func g@(AV.NodeList (_, _)) = do
  col <- g OM..> columnsAttr
  case col of
    Just (AV.Series sr) -> flip ANC.mapConcurrently_ sr $ \attr -> case attr of
      AV.Element (AV.ElemText tr) -> ONL.mapAt (T.unpack tr) func g
      _ -> return ()
    _ -> return ()
mapAtAll _ _ = return ()

headN :: Int -> OG.Graph -> IO OG.Graph
headN num g = columns g >>= \col -> g `OM.iLoc` [0..(num-1)] `ONL.getS` col

transpose :: OG.Graph -> OG.Graph
transpose (AV.Series sr) = AV.Series . V.fromList $ Prelude.map AV.fromList rows
  where
    rowsNumV = V.map OM.len sr
    rowsNum = V.foldl' Prelude.min (V.head rowsNumV) rowsNumV
    values = V.map (OM.take rowsNum) sr
    list = V.toList $ V.map AV.toList values
    rows = L.transpose list
transpose _ = AV.attrValueNan

toCSV :: FilePath -> [String] -> OG.Graph -> IO ()
toCSV path headers (AV.Series sr)
  | V.length sr == 0 = return ()
  | otherwise = TF.writeFileEnsuringDir path finalContent
  where
    rowsNumV = V.map OM.len sr
    rowsNum = V.foldl' Prelude.min (V.head rowsNumV) rowsNumV
    values = V.map (OM.take rowsNum) sr
    headerText = T.intercalate (T.pack ",") $ Prelude.map T.pack headers
    listCols = V.toList $ V.map AV.toList values
    rows = L.transpose listCols
    formatCell x = T.pack (show x)
    formatRow row = T.intercalate (T.pack ",") (Prelude.map formatCell row)
    allRows = headerText : Prelude.map formatRow rows
    finalContent = T.unlines allRows
toCSV _ _ _ = return ()
