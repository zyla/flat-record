{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
module Record.JsonSpec(spec) where

import Test.Hspec
import Data.FlatRecord
import Data.Aeson.QQ (json)

spec :: Spec
spec = do
  it "recordToJSON" $ do
    recordToJSON rnil `shouldBe` [json| {} |]
    recordToJSON (#foo =: 1 <+> #bar =: True) `shouldBe`
      [json| {"foo": 1, "bar": true} |]
