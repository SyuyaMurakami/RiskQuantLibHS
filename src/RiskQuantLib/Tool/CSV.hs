{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiskQuantLib.Tool.CSV (
  columnsAttr,
  readCSV,
  columns,
  mapAtAll,
  headN,
  setAs,
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
import qualified RiskQuantLib.Operation.Vector as OPV

import qualified GHC.Conc as GHCC
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector.Strict as V
import qualified Data.IntMap.Strict as IMap
import qualified Control.Concurrent.Async as ANC
import qualified Control.Concurrent.STM as STM

columnsAttr :: AK.AttrName
columnsAttr = "columns"

columnsFill :: V.Vector T.Text -> V.Vector T.Text
columnsFill vec = V.map fillCol (V.indexed vec)
  where
    fillCol (idx, cell)
        | T.null cell = T.pack ("col-" ++ show idx)
        | otherwise  = cell

readCSV :: FilePath -> IO OG.Graph
readCSV path = TF.readFileWithUtf8 path >>= \content -> do
  let allLines = V.filter (not . T.null) (V.fromList . T.lines $ content)
  case V.length allLines == 0 of
    True -> N.new >>= \n -> return $ AV.NodeList (n, NV.new)
    False -> do
      let headLine = V.head allLines
      let header = columnsFill . V.fromList $ T.splitOn (T.pack ",") headLine
      let headerKey = V.map AK.toAttrT header
      let valueLines = V.tail allLines
      cores <- GHCC.getNumProcessors
      let valuesBlock = OPV.splitTo valueLines cores
      vecList <- flip ANC.mapConcurrently valuesBlock $ \block -> V.forM block $ \line -> do
        let values = V.map AV.fromText . V.fromList . T.splitOn (T.pack ",") $ line
        STM.newTVarIO $ IMap.fromList . V.toList $ V.zip headerKey values
      let vec = V.concat vecList
      n <- N.new
      (N..<) n columnsAttr (AV.Series $ V.map AV.fromText header)
      return $ AV.NodeList (n, vec)

columns :: OG.Graph -> IO [AK.AttrName]
columns g@(AV.NodeList _) = do
  col <- g OM..> columnsAttr
  case col of
    Just c -> OM.for c (return . T.unpack . AV.toText) >>= return . V.toList
    Nothing -> return []
columns _ = return []

mapAtAll :: (OG.Graph -> OG.Graph) -> OG.Graph -> IO ()
mapAtAll func g@(AV.NodeList _) = do
  col <- g OM..> columnsAttr
  case col of
    Just (AV.Series sr) -> flip ANC.mapConcurrently_ sr $ \attr -> case attr of
      AV.Element (AV.ElemText tr) -> ONL.mapAt (T.unpack tr) func g
      _ -> return ()
    _ -> return ()
mapAtAll _ _ = return ()

headN :: Int -> OG.Graph -> IO OG.Graph
headN num g = columns g >>= \col -> g `OM.iLoc` [0..(num-1)] `ONL.getS` col

setAs :: OG.Graph -> AK.AttrName -> OG.Graph -> IO ()
setAs g@(AV.NodeList _) attr value = do
  col <- columns g
  ONL.set g attr value
  g OM..< columnsAttr $ AV.fromList . Prelude.map AV.fromString $ col ++ [attr]
setAs _ _ _ = return ()

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
