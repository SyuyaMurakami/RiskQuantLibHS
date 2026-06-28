{-# LANGUAGE BangPatterns #-}

module RiskQuantLib.Attribute.Key (
  AttrName,
  AttrIndex,
  toAttrT,
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

fromAttrMap :: AttrMap -> AttrKey -> IO AttrIndex
fromAttrMap attrMap attrKey = IOR.readIORef attrMap >>= \am -> 
  case SM.lookup attrKey am of
    Just v -> return v
    Nothing -> modifyAttrMap attrMap attrKey

{-# NOINLINE globalSymbolTable #-}

globalSymbolTable :: AttrMap
globalSymbolTable = unsafePerformIO $ newAttrMap

toAttrT :: AttrKey -> AttrIndex
toAttrT attrKey = unsafePerformIO $ fromAttrMap globalSymbolTable attrKey

toAttr :: AttrName -> AttrIndex
toAttr attrName = let !attrKey = T.pack attrName in toAttrT attrKey

