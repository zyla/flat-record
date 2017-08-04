module HList.BaseSpec (spec) where

import Test.Hspec
import Control.Exception
import Data.FlatHList

spec :: Spec
spec = do
  describe "hseq" $ do
    it "first element" $ do
      evaluate (hseq (hsingleton undefined))
        `shouldThrow` anyErrorCall

    it "later elements" $ do
      evaluate (hseq (hsingleton 1 `happend`
                      hsingleton 2 `happend`
                      hsingleton undefined))
        `shouldThrow` anyErrorCall
