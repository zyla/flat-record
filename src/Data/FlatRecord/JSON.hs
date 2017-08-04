{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.FlatRecord.JSON (recordToJSON) where

import qualified Data.Text as T
import Data.Proxy
import Data.FlatRecord.Base
import Data.FlatHList
import Data.Aeson
import Data.Aeson.Types
import GHC.TypeLits
import GHC.Exts

recordToJSON :: All ToJSONField xs => Record xs -> Value
recordToJSON (Record xs) = object (hcToList @ToJSONField toJSONField xs)

class ToJSONField a where
  toJSONField :: a -> Pair

instance (KnownSymbol label, ToJSON a) => ToJSONField (label :-> a) where
  toJSONField (Val x) = T.pack (symbolVal (Proxy @label)) .= x
