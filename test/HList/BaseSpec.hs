{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module HList.BaseSpec (spec) where

import Test.Hspec
import Control.Exception
import Data.FlatHList

spec :: Spec
spec = do
  describe "hseq" hseqSpec
  describe "hcgenerate" hcgenerateSpec
  describe "hcgenerate2" hcgenerate2Spec

hseqSpec = do
  it "first element" $ do
    evaluate (hseq (hsingleton undefined))
      `shouldThrow` anyErrorCall

  it "later elements" $ do
    evaluate (hseq (hsingleton 1 `happend`
                    hsingleton 2 `happend`
                    hsingleton undefined))
      `shouldThrow` anyErrorCall

type Units = '[(), (), ()]

hcgenerateSpec = do
  it "works" $ do
    let xs :: HList Units
        xs = hcgenerate @((~) ()) (\_ -> ())
    xs `shouldBe` (hsingleton () `happend` hsingleton () `happend` hsingleton ())

type Fields = '[Int, Bool]
type FieldsMaybe = '[Maybe Int, Maybe Bool]

class b ~ Maybe a => ToMaybe a b
instance b ~ Maybe a => ToMaybe a b

hcgenerate2Spec = do
  it "works" $ do
    let xs :: HList Fields
        xs = 1 `hcons` True `hcons` hnil

        ys :: HList FieldsMaybe
        ys = hcgenerate2 @ToMaybe @Fields (\i _ -> Just (hindex i xs))

    ys `shouldBe` (Just 1 `hcons` Just True `hcons` hnil)
