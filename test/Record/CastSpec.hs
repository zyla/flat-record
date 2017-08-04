{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
module Record.CastSpec where

import Test.Hspec
import Data.FlatRecord

type F1 = '[ "foo" :-> Int, "bar" :-> Bool, "baz" :-> () ]
type F2 = '[ "bar" :-> Bool, "foo" :-> Int ]

castF1F2 :: Record F1 -> Record F2
castF1F2 = rcast

spec :: Spec
spec =
  it "rcast compiles" $ do
    castF1F2 (#foo =: 1 <+> #bar =: True <+> #baz =: ())
      `shouldBe` (#bar =: True <+> #foo =: 1)
