{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
  

module Data.Aeson.Util.Test where

import           Data.Attoparsec.Number (Number(..))
import           Data.Aeson
import qualified Data.Aeson as A
import           Control.Applicative
import qualified Data.Vector as V
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Hashable (Hashable)
import           Data.Foldable
import           Data.Maybe (fromJust, fromMaybe)

import qualified Data.Aeson.Util as AU

instance Arbitrary A.Array where
  arbitrary = V.fromList <$> vectorOf 2 (arbitrary :: Gen A.Value)

instance (Eq a, Hashable a, Arbitrary a, Arbitrary b) => Arbitrary (HM.HashMap a b) where
  arbitrary = HM.fromList <$> vectorOf 2 (arbitrary :: (Arbitrary a, Arbitrary b) => Gen (a, b))

instance Arbitrary Number where
  arbitrary = oneof [I <$> arbitrary, D <$> arbitrary]

instance Arbitrary Text where
  arbitrary = T.pack <$> (arbitrary :: Gen String)

instance CoArbitrary Number where
  coarbitrary (I _) = variant 0
  coarbitrary (D _) = variant (-1)

instance CoArbitrary Object where
  coarbitrary = coarbitrary . HM.toList

instance CoArbitrary Array where
  coarbitrary a =
    case V.splitAt 1 a of
      (v1, v2)
        | V.null v1 || V.null v2 -> variant 0
        | otherwise              -> variant (-1) . coarbitrary (V.splitAt 1 v2)

instance CoArbitrary Text where
  coarbitrary t = 
    case T.uncons t of
      Just (x, xs) -> variant (-1) . coarbitrary (x, xs)
      Nothing      -> variant 0
    
instance Arbitrary A.Value where
  arbitrary = oneof [
        A.String <$> (arbitrary :: Gen Text)
      , A.Array  <$> (arbitrary :: Gen Array)
      , A.Object <$> (arbitrary :: Gen Object)
      , A.Number <$> (arbitrary :: Gen Number)
      , A.Bool   <$> (arbitrary :: Gen Bool)
      , return A.Null
    ]

  shrink (A.Array a)  = fromMaybe [] (a V.!? 0 >>= return . (:[]))
  shrink (A.Object o) = []
  shrink c = []

instance CoArbitrary A.Value where
  coarbitrary A.Null       = variant 0
  coarbitrary (A.String a) = variant 1 . coarbitrary a
  coarbitrary (A.Array  a) = variant 2 . coarbitrary a
  coarbitrary (A.Object a) = variant 3 . coarbitrary a
  coarbitrary (A.Number a) = variant 4 . coarbitrary a
  coarbitrary (A.Bool a)   = variant 5 . coarbitrary a

wrap v = toJSON [v]

prop_serial :: Value -> Bool
prop_serial v' = Just v == (decode . encode) v
  where
    v = wrap v'

prop_pure_traversal_is_map :: Blind (Value -> Value) -> Value -> Bool
prop_pure_traversal_is_map (Blind fn) v =
  fromJust (AU.traverse (pure . fn) v) == AU.map fn v
