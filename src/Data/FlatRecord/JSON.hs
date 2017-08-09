{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Data.FlatRecord.JSON (
    recordToJSON
  , RecordToJSON
  , recordParseJSON
  , RecordFromJSON
) where

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

class RecordFromJSON rs where
  parseRecordFromObject :: Object -> Parser (Record rs)

instance RecordFromJSON '[] where
  parseRecordFromObject _ = pure rnil

instance (KnownSymbol label, FromJSON a, RecordFromJSON rs)
    => RecordFromJSON ((label :-> a) : rs) where
  parseRecordFromObject object =
    rcons
      <$> object .: T.pack (symbolVal (Proxy @label))
      <*> parseRecordFromObject object

recordParseJSON :: RecordFromJSON rs => Value -> Parser (Record rs)
recordParseJSON = withObject "Record" parseRecordFromObject
