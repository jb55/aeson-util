
module Data.Aeson.Util (
  traverseWithKey
, traverse
, map
) where

import           Data.Aeson
import           Prelude hiding (map)
import qualified Prelude as P
import           Data.Text (Text)
import           Control.Applicative
import qualified Data.Traversable as Traversable
import qualified Data.HashMap.Lazy as Map

-- | Transform Values by accumulating an Applicative result from every value.
-- The first argument to transforming function will be Nothing if it is not an
-- Object's key
traverseWithKey :: Applicative f => (Maybe Text -> Value -> f Value) -> Value -> f Value
traverseWithKey fn = go
  where
    go (Array a)  = Array  <$> Traversable.traverse go a
    go (Object o) = Object <$> Map.traverseWithKey (fn . Just) o
    go v          = fn Nothing v

traverse :: Applicative f => (Value -> f Value) -> Value -> f Value
traverse f = traverseWithKey $ const f

map :: (Value -> Value) -> Value -> Value
map fn = go
  where
    go (Array xs) = Array $ fmap go xs
    go (Object x) = Object $ Map.map go x
    go v          = fn v

adjust :: (Value -> Value) -> Value -> Value
adjust = map
