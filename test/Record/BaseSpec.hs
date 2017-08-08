{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Record.BaseSpec where

import Test.Hspec
import Data.FlatRecord

spec :: Spec
spec = do
  it "show does not have ugly ornaments" $ do
    show (#foo =: ()) `shouldBe` "[foo :-> ()]"

  rcgenerate2Spec
  rcmapSpec

type Fields = '["foo" :-> Int, "bar" :-> Bool]
type FieldsMaybe = '["foo" :-> Maybe Int, "bar" :-> Maybe Bool]

class b ~ Maybe a => ToMaybe a b
instance b ~ Maybe a => ToMaybe a b

rcgenerate2Spec =
  it "works" $ do
    let xs :: Record Fields
        xs = #foo =: 1 <+> #bar =: True

        ys :: Record FieldsMaybe
        ys = rcgenerate2 @ToMaybe @Fields (\i _ -> Just (rindex i xs))

    ys `shouldBe` (#foo =: Just 1 <+> #bar =: Just True)

rcmapSpec =
  it "works" $ do
    rcmap @ToMaybe Just (#foo =: 1 <+> #bar =: True)
      `shouldBe` (#foo =: Just 1 <+> #bar =: Just True)
