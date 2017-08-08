{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
module Record.BaseSpec where

import Test.Hspec
import Data.FlatRecord

spec :: Spec
spec =
  it "show does not have ugly ornaments" $ do
    show (#foo =: ()) `shouldBe` "[foo :-> ()]"
