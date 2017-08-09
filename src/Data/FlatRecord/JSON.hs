{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.FlatRecord.JSON (recordToJSON, RecordToJSON) where

import qualified Data.Text as T
import Data.Proxy
import Data.FlatRecord.Base
import Data.FlatHList
import Data.Aeson
import Data.Aeson.Types
import GHC.TypeLits
import GHC.Exts

type RecordToJSON rs = All ToJSONField rs

recordToJSON :: RecordToJSON xs => Record xs -> Value
recordToJSON (Record xs) = object (hcToList @ToJSONField toJSONField xs)

class ToJSONField a where
  toJSONField :: a -> Pair

instance (KnownSymbol label, ToJSON a) => ToJSONField (label :-> a) where
  toJSONField (Val x) = T.pack (symbolVal (Proxy @label)) .= x
