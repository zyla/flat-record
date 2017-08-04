{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
module Record.JsonSpec(spec) where

import Test.Hspec
import Data.FlatRecord
import Data.FlatRecord.JSON
import Data.Aeson.QQ (aesonQQ)

spec :: Spec
spec = do
  it "recordToJSON" $ do
    recordToJSON rnil `shouldBe` [aesonQQ| {} |]
    recordToJSON (#foo =: (1 :: Int) <+> #bar =: True) `shouldBe`
      [aesonQQ| {"foo": 1, "bar": true} |]
