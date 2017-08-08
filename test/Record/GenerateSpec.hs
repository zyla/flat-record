{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
module Record.GenerateSpec where

import Test.Hspec
import Data.FlatRecord

type F1 = '[ "foo" :-> String, "bar" :-> [Int], "baz" :-> () ]

memptyF1 :: Record F1
memptyF1 = rcgenerate @Monoid (\_ -> mempty)

type Nums = '[ "int" :-> Int, "float" :-> Float ]

nums :: Record Nums
nums = rcgenerate @Num (\_ -> 5)

spec :: Spec
spec =
  it "rcgenerate" $ do
    memptyF1 `shouldBe` (#foo =: "" <+> #bar =: [] <+> #baz =: ())
    nums `shouldBe` (#int =: 5 <+> #float =: 5)
