{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.FlatHList
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl.Functor
import Data.Vinyl (Rec((:&), RNil))
import Criterion.Main

flatGet :: HList Fields -> Int
flatGet = hget

vinylGet :: V.HList Fields -> Int
vinylGet = getIdentity . V.rget (Proxy @Int)

plainGet :: PlainRecord -> Int
plainGet = field81

main = defaultMain
  [ bench "FlatHList" $ nf flatGet flatHList
  , bench "Vinyl" $ nf vinylGet vinylHList
  , bench "PlainRecord" $ nf field81 plainRecord
  ]


type Fields = 
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Bool :
  Int :
  '[]

flatHList :: HList Fields
flatHList =
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton True `happend`
  hsingleton 1 `happend`
  hnil

vinylHList :: V.HList Fields
vinylHList =
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity True :&
  Identity 1 :&
  RNil

data PlainRecord = PlainRecord
  { field0 :: Bool
  , field1 :: Bool
  , field2 :: Bool
  , field3 :: Bool
  , field4 :: Bool
  , field5 :: Bool
  , field6 :: Bool
  , field7 :: Bool
  , field8 :: Bool
  , field9 :: Bool
  , field10 :: Bool
  , field11 :: Bool
  , field12 :: Bool
  , field13 :: Bool
  , field14 :: Bool
  , field15 :: Bool
  , field16 :: Bool
  , field17 :: Bool
  , field18 :: Bool
  , field19 :: Bool
  , field20 :: Bool
  , field21 :: Bool
  , field22 :: Bool
  , field23 :: Bool
  , field24 :: Bool
  , field25 :: Bool
  , field26 :: Bool
  , field27 :: Bool
  , field28 :: Bool
  , field29 :: Bool
  , field30 :: Bool
  , field31 :: Bool
  , field32 :: Bool
  , field33 :: Bool
  , field34 :: Bool
  , field35 :: Bool
  , field36 :: Bool
  , field37 :: Bool
  , field38 :: Bool
  , field39 :: Bool
  , field40 :: Bool
  , field41 :: Bool
  , field42 :: Bool
  , field43 :: Bool
  , field44 :: Bool
  , field45 :: Bool
  , field46 :: Bool
  , field47 :: Bool
  , field48 :: Bool
  , field49 :: Bool
  , field50 :: Bool
  , field51 :: Bool
  , field52 :: Bool
  , field53 :: Bool
  , field54 :: Bool
  , field55 :: Bool
  , field56 :: Bool
  , field57 :: Bool
  , field58 :: Bool
  , field59 :: Bool
  , field60 :: Bool
  , field61 :: Bool
  , field62 :: Bool
  , field63 :: Bool
  , field64 :: Bool
  , field65 :: Bool
  , field66 :: Bool
  , field67 :: Bool
  , field68 :: Bool
  , field69 :: Bool
  , field70 :: Bool
  , field71 :: Bool
  , field72 :: Bool
  , field73 :: Bool
  , field74 :: Bool
  , field75 :: Bool
  , field76 :: Bool
  , field77 :: Bool
  , field78 :: Bool
  , field79 :: Bool
  , field80 :: Bool
  , field81 :: Int
  }

plainRecord :: PlainRecord
plainRecord = PlainRecord
  { field0 = True
  , field1 = True
  , field2 = True
  , field3 = True
  , field4 = True
  , field5 = True
  , field6 = True
  , field7 = True
  , field8 = True
  , field9 = True
  , field10 = True
  , field11 = True
  , field12 = True
  , field13 = True
  , field14 = True
  , field15 = True
  , field16 = True
  , field17 = True
  , field18 = True
  , field19 = True
  , field20 = True
  , field21 = True
  , field22 = True
  , field23 = True
  , field24 = True
  , field25 = True
  , field26 = True
  , field27 = True
  , field28 = True
  , field29 = True
  , field30 = True
  , field31 = True
  , field32 = True
  , field33 = True
  , field34 = True
  , field35 = True
  , field36 = True
  , field37 = True
  , field38 = True
  , field39 = True
  , field40 = True
  , field41 = True
  , field42 = True
  , field43 = True
  , field44 = True
  , field45 = True
  , field46 = True
  , field47 = True
  , field48 = True
  , field49 = True
  , field50 = True
  , field51 = True
  , field52 = True
  , field53 = True
  , field54 = True
  , field55 = True
  , field56 = True
  , field57 = True
  , field58 = True
  , field59 = True
  , field60 = True
  , field61 = True
  , field62 = True
  , field63 = True
  , field64 = True
  , field65 = True
  , field66 = True
  , field67 = True
  , field68 = True
  , field69 = True
  , field70 = True
  , field71 = True
  , field72 = True
  , field73 = True
  , field74 = True
  , field75 = True
  , field76 = True
  , field77 = True
  , field78 = True
  , field79 = True
  , field80 = True
  , field81 = 1
  }
