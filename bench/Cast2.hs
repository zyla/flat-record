{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=1000 -ddump-simpl-stats #-}

module Main (main) where

import Data.FlatHList
import Data.Proxy
import qualified Data.Vinyl as V
import Data.Vinyl.Functor
import Data.Vinyl (Rec((:&), RNil))
import Control.DeepSeq
import Criterion.Main

flatCast :: HList Fields -> HList Fields2
flatCast = hcast

vinylCast :: V.HList Fields -> V.HList Fields2
vinylCast = V.rcast

main = defaultMain
  [ bench "FlatHList" $ whnf (hseq . flatCast) flatHList
  , bench "Vinyl" $ whnf vinylCast vinylHList
  ]

newtype Phantom a = Phantom { unPhantom :: Int } deriving (NFData)

type Fields = 
  Phantom 0 :
  Phantom 1 :
  Phantom 2 :
  Phantom 3 :
  Phantom 4 :
  Phantom 5 :
  Phantom 6 :
  Phantom 7 :
  Phantom 8 :
  Phantom 9 :
  Phantom 10 :
  Phantom 11 :
  '[]

type Fields2 = 
  Phantom 11 :
  Phantom 10 :
  Phantom 9 :
  Phantom 8 :
  Phantom 7 :
  Phantom 6 :
  Phantom 5 :
  Phantom 4 :
  Phantom 3 :
  Phantom 2 :
  Phantom 1 :
  Phantom 0 :
  '[]

flatHList :: HList Fields
flatHList =
  hsingleton (Phantom 0) `happend`
  hsingleton (Phantom 1) `happend`
  hsingleton (Phantom 2) `happend`
  hsingleton (Phantom 3) `happend`
  hsingleton (Phantom 4) `happend`
  hsingleton (Phantom 5) `happend`
  hsingleton (Phantom 6) `happend`
  hsingleton (Phantom 7) `happend`
  hsingleton (Phantom 8) `happend`
  hsingleton (Phantom 9) `happend`
  hsingleton (Phantom 10) `happend`
  hsingleton (Phantom 11) `happend`
  hnil

vinylHList :: V.HList Fields
vinylHList =
  Identity (Phantom 0) :&
  Identity (Phantom 1) :&
  Identity (Phantom 2) :&
  Identity (Phantom 3) :&
  Identity (Phantom 4) :&
  Identity (Phantom 5) :&
  Identity (Phantom 6) :&
  Identity (Phantom 7) :&
  Identity (Phantom 8) :&
  Identity (Phantom 9) :&
  Identity (Phantom 10) :&
  Identity (Phantom 11) :&
  RNil
