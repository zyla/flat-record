{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Data.FlatHList
import Criterion.Main

flatCast3 :: HList Fields3 -> HList Fields3
flatCast3 = hcast

flatCast10 :: HList Fields10 -> HList Fields10
flatCast10 = hcast

flatCast30 :: HList Fields30 -> HList Fields30
flatCast30 = hcast

main = defaultMain
  [ bench "3"  $ whnf (hseq . flatCast3) flatHList3
  , bench "10" $ whnf (hseq . flatCast10) flatHList10
  , bench "30" $ whnf (hseq . flatCast30) flatHList30
  ]

newtype Phantom a = Phantom { unPhantom :: Int }

type Fields3 = 
  Phantom 0 :
  Phantom 1 :
  Phantom 2 :
  '[]

flatHList3 :: HList Fields3
flatHList3 =
  hsingleton (Phantom 0) `happend`
  hsingleton (Phantom 1) `happend`
  hsingleton (Phantom 2) `happend`
  hnil

type Fields10 = 
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
  '[]

flatHList10 :: HList Fields10
flatHList10 =
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
  hnil

type Fields30 = 
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
  Phantom 12 :
  Phantom 13 :
  Phantom 14 :
  Phantom 15 :
  Phantom 16 :
  Phantom 17 :
  Phantom 18 :
  Phantom 19 :
  Phantom 20 :
  Phantom 21 :
  Phantom 22 :
  Phantom 23 :
  Phantom 24 :
  Phantom 25 :
  Phantom 26 :
  Phantom 27 :
  Phantom 28 :
  Phantom 29 :
  '[]

flatHList30 :: HList Fields30
flatHList30 =
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
  hsingleton (Phantom 12) `happend`
  hsingleton (Phantom 13) `happend`
  hsingleton (Phantom 14) `happend`
  hsingleton (Phantom 15) `happend`
  hsingleton (Phantom 16) `happend`
  hsingleton (Phantom 17) `happend`
  hsingleton (Phantom 18) `happend`
  hsingleton (Phantom 19) `happend`
  hsingleton (Phantom 20) `happend`
  hsingleton (Phantom 21) `happend`
  hsingleton (Phantom 22) `happend`
  hsingleton (Phantom 23) `happend`
  hsingleton (Phantom 24) `happend`
  hsingleton (Phantom 25) `happend`
  hsingleton (Phantom 26) `happend`
  hsingleton (Phantom 27) `happend`
  hsingleton (Phantom 28) `happend`
  hsingleton (Phantom 29) `happend`
  hnil
