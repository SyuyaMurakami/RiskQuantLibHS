{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module RiskQuantLib.Attribute.Value (
  ElementValue(..),
  AttrValue(..),
  elementValueNan,
  attrValueNan,
  is,
  isNot,
  asInt,
  asDouble,
  asText,
  asBool,
  fromInt,
  fromDouble,
  fromText,
  fromBool,
  fromString,
  fromList,
  fromVector,
  toInt,
  toDouble,
  toText,
  toString,
  toBool,
  toList,
  toVector,
  isInt,
  isDouble,
  isText,
  isBool,
  isSeries,
  isNodeList,
  nan,
  isNan,
  notNan
) where

import qualified RiskQuantLib.Node.Node as N
import qualified RiskQuantLib.Node.NodeVector as NV

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector.Strict as V

import GHC.Generics (Generic)
import Data.Ratio (numerator, denominator)
import Data.Hashable (Hashable)
import Text.Read (readMaybe)

data ElementValue = ElemInt Int | ElemDouble Double | ElemText T.Text | ElemBool Bool deriving (Generic)

elementValueNan :: ElementValue
elementValueNan = ElemText T.empty

instance Show ElementValue where
  show (ElemInt a) = Prelude.show a
  show (ElemDouble a) = Prelude.show a
  show (ElemText a) = if a == T.empty then "" else T.unpack a
  show (ElemBool a) = Prelude.show a

instance Ord ElementValue where
  compare (ElemInt a) (ElemInt b) = compare a b
  compare (ElemDouble a) (ElemDouble b) = compare a b
  compare (ElemText a) (ElemText b) = compare a b
  compare (ElemBool a) (ElemBool b) = compare a b
  compare (ElemInt a) (ElemDouble b) = compare (fromIntegral a) b
  compare (ElemDouble a) (ElemInt b) = compare a (fromIntegral b)
  compare (ElemInt _) (ElemText _) = LT
  compare (ElemText _) (ElemInt _) = GT
  compare (ElemInt _) (ElemBool _) = LT
  compare (ElemBool _) (ElemInt _) = GT
  compare (ElemDouble _) (ElemText _) = LT
  compare (ElemText _) (ElemDouble _) = GT
  compare (ElemDouble _) (ElemBool _) = LT
  compare (ElemBool _) (ElemDouble _) = GT
  compare (ElemText _) (ElemBool _) = LT
  compare (ElemBool _) (ElemText _) = GT

instance Eq ElementValue where
  a == b = compare a b == EQ

instance Num ElementValue where
  (ElemInt a) + (ElemInt b) = ElemInt (a + b)
  (ElemDouble a) + (ElemDouble b) = ElemDouble (a + b)
  (ElemInt a) + (ElemDouble b) = ElemDouble (fromIntegral a + b)
  (ElemDouble a) + (ElemInt b) = ElemDouble (a + (fromIntegral b))
  (ElemText a) + (ElemText b) = ElemText (a <> b) 
  (ElemBool a) + (ElemBool b) = ElemBool (a || b)
  _ + _ = elementValueNan

  (ElemInt a) * (ElemInt b) = ElemInt (a * b)
  (ElemDouble a) * (ElemDouble b) = ElemDouble (a * b)
  (ElemBool a) * (ElemBool b) = ElemBool (a && b)
  (ElemInt a) * (ElemDouble b) = ElemDouble (fromIntegral a * b)
  (ElemDouble a) * (ElemInt b) = ElemDouble (a * (fromIntegral b))
  _ * _ = elementValueNan

  negate (ElemInt a) = ElemInt (negate a)
  negate (ElemDouble a) = ElemDouble (negate a)
  negate (ElemText _) = elementValueNan
  negate (ElemBool a) = ElemBool $ not a

  abs (ElemInt a) = ElemInt (abs a)
  abs (ElemDouble a) = ElemDouble (abs a)
  abs _ = elementValueNan

  signum (ElemInt a) = ElemInt (signum a)
  signum (ElemDouble a) = ElemDouble (signum a)
  signum _ = elementValueNan

  fromInteger n = ElemDouble $ fromIntegral n

instance Fractional ElementValue where
  (ElemInt a) / (ElemInt b) = ElemDouble $ (fromIntegral a) / (fromIntegral b)
  (ElemDouble a) / (ElemDouble b) = ElemDouble $ a / b
  (ElemInt a) / (ElemDouble b) = ElemDouble $ (fromIntegral a) / b
  (ElemDouble a) / (ElemInt b) = ElemDouble $ a / (fromIntegral b)
  _ / _ = elementValueNan

  fromRational r = ElemDouble $ fromIntegral (numerator r) / fromIntegral (denominator r)

instance Floating ElementValue where
  pi = ElemDouble pi

  exp (ElemInt a) = ElemDouble $ exp (fromIntegral a)
  exp (ElemDouble a) = ElemDouble $ exp a
  exp _ = elementValueNan

  log (ElemInt a) = ElemDouble $ log (fromIntegral a)
  log (ElemDouble a) = ElemDouble $ log a
  log _ = elementValueNan

  sin (ElemInt a) = ElemDouble $ sin (fromIntegral a)
  sin (ElemDouble a) = ElemDouble $ sin a
  sin _ = elementValueNan

  cos (ElemInt a) = ElemDouble $ cos (fromIntegral a)
  cos (ElemDouble a) = ElemDouble $ cos a
  cos _ = elementValueNan

  asin (ElemInt a) = ElemDouble $ asin (fromIntegral a)
  asin (ElemDouble a) = ElemDouble $ asin a
  asin _ = elementValueNan

  acos (ElemInt a) = ElemDouble $ acos (fromIntegral a)
  acos (ElemDouble a) = ElemDouble $ acos a
  acos _ = elementValueNan

  atan (ElemInt a) = ElemDouble $ atan (fromIntegral a)
  atan (ElemDouble a) = ElemDouble $ atan a
  atan _ = elementValueNan

  sinh (ElemInt a) = ElemDouble $ sinh (fromIntegral a)
  sinh (ElemDouble a) = ElemDouble $ sinh a
  sinh _ = elementValueNan

  cosh (ElemInt a) = ElemDouble $ cosh (fromIntegral a)
  cosh (ElemDouble a) = ElemDouble $ cosh a
  cosh _ = elementValueNan

  asinh (ElemInt a) = ElemDouble $ asinh (fromIntegral a)
  asinh (ElemDouble a) = ElemDouble $ asinh a
  asinh _ = elementValueNan

  acosh (ElemInt a) = ElemDouble $ acosh (fromIntegral a)
  acosh (ElemDouble a) = ElemDouble $ acosh a
  acosh _ = elementValueNan

  atanh (ElemInt a) = ElemDouble $ atanh (fromIntegral a)
  atanh (ElemDouble a) = ElemDouble $ atanh a
  atanh _ = elementValueNan

instance Hashable ElementValue

data AttrValue = Element ElementValue
  | Series (V.Vector AttrValue)
  | Node (N.Node AttrValue)
  | NodeList (N.Node AttrValue, NV.NodeVector AttrValue)

attrValueNan :: AttrValue
attrValueNan = Element elementValueNan

instance Show AttrValue where
  show (Element ele) = show ele
  show (Series sr) = Prelude.show sr
  show (Node _) = "Node"
  show (NodeList _) = "NodeList"

instance Ord AttrValue where
  compare (Element a) (Element b) = Prelude.compare a b
  compare (Series _) (Series _) = EQ
  compare (Node _) (Node _) = EQ
  compare (NodeList _) (NodeList _) = EQ
  compare (Element _) _ = LT
  compare _ (Element _) = GT
  compare (Series _) _ = LT
  compare _ (Series _) = GT
  compare (Node _) _ = LT
  compare _ (Node _) = GT

instance Eq AttrValue where
  a == b = compare a b == EQ

instance Num AttrValue where
  (Series a) + (Series b) = Series $ V.zipWith (+) a b
  (Series a) + b = Series $ V.map (+b) a
  b + (Series a) = Series $ V.map (b+) a
  (Element a) + (Element b) = Element $ a + b
  (NodeList (qA, nvcA)) + (NodeList (_, nvcB)) = NodeList (qA, NV.add nvcA nvcB)
  _ + _ = attrValueNan

  (Series a) * (Series b) = Series $ V.zipWith (*) a b
  (Series a) * b = Series $ V.map (*b) a
  b * (Series a) = Series $ V.map (b*) a
  (Element a) * (Element b) = Element $ a * b
  _ * _ = attrValueNan

  negate (Series a) = Series $ V.map negate a
  negate (Element a) = Element $ negate a
  negate _ = attrValueNan

  abs (Series a) = Series $ V.map abs a
  abs (Element a) = Element $ abs a
  abs _ = attrValueNan

  signum (Series a) = Series $ V.map signum a
  signum (Element a) = Element $ signum a
  signum _ = attrValueNan

  fromInteger n = Element $ fromInteger n

instance Fractional AttrValue where
  (Series a) / (Series b) = Series $ V.zipWith (/) a b
  (Series a) / b = Series $ V.map (/b) a
  b / (Series a) = Series $ V.map (b/) a
  (Element a) / (Element b) = Element $ a / b
  _ / _ = attrValueNan

  fromRational r = Element . ElemDouble $ fromIntegral (numerator r) / fromIntegral (denominator r)

instance Floating AttrValue where
  pi = Element (ElemDouble pi)

  exp (Element a) = Element $ exp a
  exp (Series a) = Series $ V.map exp a
  exp _ = attrValueNan

  log (Element a) = Element $ log a
  log (Series a) = Series $ V.map log a
  log _ = attrValueNan

  sin (Element a) = Element $ sin a
  sin (Series a) = Series $ V.map sin a
  sin _ = attrValueNan

  cos (Element a) = Element $ cos a
  cos (Series a) = Series $ V.map cos a
  cos _ = attrValueNan

  asin (Element a) = Element $ asin a
  asin (Series a) = Series $ V.map asin a
  asin _ = attrValueNan

  acos (Element a) = Element $ acos a
  acos (Series a) = Series $ V.map acos a
  acos _ = attrValueNan

  atan (Element a) = Element $ atan a
  atan (Series a) = Series $ V.map atan a
  atan _ = attrValueNan

  sinh (Element a) = Element $ sinh a
  sinh (Series a) = Series $ V.map sinh a
  sinh _ = attrValueNan

  cosh (Element a) = Element $ cosh a
  cosh (Series a) = Series $ V.map cosh a
  cosh _ = attrValueNan

  asinh (Element a) = Element $ asinh a
  asinh (Series a) = Series $ V.map asinh a
  asinh _ = attrValueNan

  acosh (Element a) = Element $ acosh a
  acosh (Series a) = Series $ V.map acosh a
  acosh _ = attrValueNan

  atanh (Element a) = Element $ atanh a
  atanh (Series a) = Series $ V.map atanh a
  atanh _ = attrValueNan

is :: AttrValue -> AttrValue -> Bool
is (Element a) (Element b) = a == b
is (Series a) (Series b) = a == b
is (Node a) (Node b) = a == b
is (NodeList (na, nvca)) (NodeList (nb, nvcb)) = (na == nb) && (nvca == nvcb)
is _ _ = False

isNot :: AttrValue -> AttrValue -> Bool
isNot a b = not $ is a b

asInt :: AttrValue -> AttrValue
asInt (Series vec) = Series $ V.map asInt vec
asInt v@(Element (ElemInt _)) = v
asInt (Element (ElemBool b)) = Element (ElemInt $ if b then 1 else 0)
asInt (Element (ElemText tx)) = case TR.decimal tx of
  Right (val, _) -> Element (ElemInt val)
  Left _      -> attrValueNan
asInt _ = attrValueNan

asDouble :: AttrValue -> AttrValue
asDouble (Series vec) = Series $ V.map asDouble vec
asDouble v@(Element (ElemDouble _)) = v
asDouble (Element (ElemBool b)) = Element (ElemDouble $ if b then 1 else 0)
asDouble (Element (ElemText tx)) = case TR.double tx of
  Right (val, _) -> Element (ElemDouble val)
  Left _      -> attrValueNan
asDouble _ = attrValueNan

asText :: AttrValue -> AttrValue
asText (Series vec) = Series $ V.map asText vec
asText v@(Element (ElemText _)) = v
asText v = Element . ElemText . T.pack $ show v

asBool :: AttrValue -> AttrValue
asBool (Series vec) = Series $ V.map asBool vec
asBool v@(Element (ElemBool _)) = v
asBool (Element (ElemInt b)) = Element $ ElemBool (if b == 0 then False else True)
asBool (Element (ElemDouble b)) = Element $ ElemBool (if b == 0 then False else True)
asBool (Element (ElemText tx)) = case readMaybe (T.unpack tx) of
  Just b -> Element (ElemBool b)
  Nothing -> attrValueNan
asBool _ = attrValueNan

fromInt :: Int -> AttrValue
fromInt = Element . ElemInt

fromDouble :: Double -> AttrValue
fromDouble = Element . ElemDouble

fromText :: T.Text -> AttrValue
fromText = Element . ElemText

fromBool :: Bool -> AttrValue
fromBool = Element . ElemBool

fromString :: String -> AttrValue
fromString = fromText . T.pack 

fromList :: [AttrValue] -> AttrValue
fromList = Series . V.fromList

fromVector :: V.Vector AttrValue -> AttrValue
fromVector = Series

toInt :: AttrValue -> Int
toInt (Element (ElemInt v)) = v
toInt _ = 0

toDouble :: AttrValue -> Double
toDouble (Element (ElemDouble v)) = v
toDouble _ = 0

toText :: AttrValue -> T.Text
toText (Element (ElemText v)) = v
toText _ = T.empty

toString :: AttrValue -> String
toString = T.unpack . toText

toBool :: AttrValue -> Bool
toBool (Element (ElemBool v)) = v
toBool _ = False

toVector :: AttrValue -> V.Vector AttrValue
toVector (Series sr) = sr
toVector (NodeList (_, nvc)) = V.map Node nvc
toVector _ = V.empty

toList :: AttrValue -> [AttrValue]
toList g = V.toList $ toVector g

isInt :: AttrValue -> Bool
isInt (Element (ElemInt _)) = True
isInt _ = False

isDouble :: AttrValue -> Bool
isDouble (Element (ElemDouble _)) = True
isDouble _ = False

isText :: AttrValue -> Bool
isText (Element (ElemText _)) = True
isText _ = False

isBool :: AttrValue -> Bool
isBool (Element (ElemBool _)) = True
isBool _ = False

isSeries :: AttrValue -> Bool
isSeries (Series _) = True
isSeries _ = False

isNodeList :: AttrValue -> Bool
isNodeList (NodeList _) = True
isNodeList _ = False

nan :: AttrValue
nan = attrValueNan

isNan :: AttrValue -> Bool
isNan v = v == attrValueNan

notNan :: AttrValue -> Bool
notNan v = v /= attrValueNan
