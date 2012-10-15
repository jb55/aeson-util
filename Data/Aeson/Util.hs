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
traverseWithKey fn = go Nothing
  where
    go _ (Array a)  = Array  <$> Traversable.traverse (go Nothing) a
    go _ (Object o) = Object <$> Map.traverseWithKey (go . Just) o
    go mt v         = fn mt v

traverse :: Applicative f => (Value -> f Value) -> Value -> f Value
traverse fn = traverseWithKey $ const fn

map :: (Value -> Value) -> Value -> Value
map fn = go
  where
    go (Array xs) = Array $ fmap go xs
    go (Object x) = Object $ Map.map go x
    go v          = fn v

adjust :: (Value -> Value) -> Value -> Value
adjust = map
