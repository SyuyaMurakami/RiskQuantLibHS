{-# LANGUAGE BangPatterns #-}

module RiskQuantLib.Attribute.Key (
  AttrName,
  AttrIndex,
  toAttr
) where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.IORef as IOR
import qualified Data.Text as T
import qualified Data.Map.Strict as SM

type AttrName = String
type AttrKey = T.Text
type AttrIndex = Int
type AttrMap = IOR.IORef (SM.Map AttrKey AttrIndex)

newAttrMap :: IO AttrMap
newAttrMap = IOR.newIORef SM.empty

modifyAttrMap :: AttrMap -> AttrKey -> IO AttrIndex
modifyAttrMap attrMap attrKey = IOR.atomicModifyIORef' attrMap $ \am -> 
  case SM.lookup attrKey am of
    Just v -> (am, v)
    Nothing -> let !sz = SM.size am in (SM.insert attrKey sz am, sz)

fromAttrMap :: AttrMap -> AttrName -> IO AttrIndex
fromAttrMap attrMap attrName = IOR.readIORef attrMap >>= \am -> 
  let !an = T.pack attrName in case SM.lookup an am of
    Just v -> return v
    Nothing -> modifyAttrMap attrMap an

{-# NOINLINE globalSymbolTable #-}

globalSymbolTable :: AttrMap
globalSymbolTable = unsafePerformIO $ newAttrMap

toAttr :: AttrName -> AttrIndex
toAttr attrName = unsafePerformIO $ fromAttrMap globalSymbolTable attrName

