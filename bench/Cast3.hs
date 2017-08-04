{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -dshow-passes #-}

module Main (main) where

import Data.FlatHList
import Criterion.Main

flatCast10 :: HList Fields10 -> HList Fields10
flatCast10 = hcast

flatCast30 :: HList Fields30 -> HList Fields30
flatCast30 = hcast

flatCast100 :: HList Fields100 -> HList Fields100
flatCast100 = hcast

main = defaultMain
  [ bench "10"  $ whnf (hseq . flatCast10) flatHList10
  , bench "30"  $ whnf (hseq . flatCast30) flatHList30
  , bench "100" $ whnf (hseq . flatCast100) flatHList100
  ]

newtype Phantom a = Phantom { unPhantom :: Int }

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

type Fields100 = 
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
  Phantom 30 :
  Phantom 31 :
  Phantom 32 :
  Phantom 33 :
  Phantom 34 :
  Phantom 35 :
  Phantom 36 :
  Phantom 37 :
  Phantom 38 :
  Phantom 39 :
  Phantom 40 :
  Phantom 41 :
  Phantom 42 :
  Phantom 43 :
  Phantom 44 :
  Phantom 45 :
  Phantom 46 :
  Phantom 47 :
  Phantom 48 :
  Phantom 49 :
  Phantom 50 :
  Phantom 51 :
  Phantom 52 :
  Phantom 53 :
  Phantom 54 :
  Phantom 55 :
  Phantom 56 :
  Phantom 57 :
  Phantom 58 :
  Phantom 59 :
  Phantom 60 :
  Phantom 61 :
  Phantom 62 :
  Phantom 63 :
  Phantom 64 :
  Phantom 65 :
  Phantom 66 :
  Phantom 67 :
  Phantom 68 :
  Phantom 69 :
  Phantom 70 :
  Phantom 71 :
  Phantom 72 :
  Phantom 73 :
  Phantom 74 :
  Phantom 75 :
  Phantom 76 :
  Phantom 77 :
  Phantom 78 :
  Phantom 79 :
  Phantom 80 :
  Phantom 81 :
  Phantom 82 :
  Phantom 83 :
  Phantom 84 :
  Phantom 85 :
  Phantom 86 :
  Phantom 87 :
  Phantom 88 :
  Phantom 89 :
  Phantom 90 :
  Phantom 91 :
  Phantom 92 :
  Phantom 93 :
  Phantom 94 :
  Phantom 95 :
  Phantom 96 :
  Phantom 97 :
  Phantom 98 :
  Phantom 99 :
  '[]

flatHList100 :: HList Fields100
flatHList100 =
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
  hsingleton (Phantom 30) `happend`
  hsingleton (Phantom 31) `happend`
  hsingleton (Phantom 32) `happend`
  hsingleton (Phantom 33) `happend`
  hsingleton (Phantom 34) `happend`
  hsingleton (Phantom 35) `happend`
  hsingleton (Phantom 36) `happend`
  hsingleton (Phantom 37) `happend`
  hsingleton (Phantom 38) `happend`
  hsingleton (Phantom 39) `happend`
  hsingleton (Phantom 40) `happend`
  hsingleton (Phantom 41) `happend`
  hsingleton (Phantom 42) `happend`
  hsingleton (Phantom 43) `happend`
  hsingleton (Phantom 44) `happend`
  hsingleton (Phantom 45) `happend`
  hsingleton (Phantom 46) `happend`
  hsingleton (Phantom 47) `happend`
  hsingleton (Phantom 48) `happend`
  hsingleton (Phantom 49) `happend`
  hsingleton (Phantom 50) `happend`
  hsingleton (Phantom 51) `happend`
  hsingleton (Phantom 52) `happend`
  hsingleton (Phantom 53) `happend`
  hsingleton (Phantom 54) `happend`
  hsingleton (Phantom 55) `happend`
  hsingleton (Phantom 56) `happend`
  hsingleton (Phantom 57) `happend`
  hsingleton (Phantom 58) `happend`
  hsingleton (Phantom 59) `happend`
  hsingleton (Phantom 60) `happend`
  hsingleton (Phantom 61) `happend`
  hsingleton (Phantom 62) `happend`
  hsingleton (Phantom 63) `happend`
  hsingleton (Phantom 64) `happend`
  hsingleton (Phantom 65) `happend`
  hsingleton (Phantom 66) `happend`
  hsingleton (Phantom 67) `happend`
  hsingleton (Phantom 68) `happend`
  hsingleton (Phantom 69) `happend`
  hsingleton (Phantom 70) `happend`
  hsingleton (Phantom 71) `happend`
  hsingleton (Phantom 72) `happend`
  hsingleton (Phantom 73) `happend`
  hsingleton (Phantom 74) `happend`
  hsingleton (Phantom 75) `happend`
  hsingleton (Phantom 76) `happend`
  hsingleton (Phantom 77) `happend`
  hsingleton (Phantom 78) `happend`
  hsingleton (Phantom 79) `happend`
  hsingleton (Phantom 80) `happend`
  hsingleton (Phantom 81) `happend`
  hsingleton (Phantom 82) `happend`
  hsingleton (Phantom 83) `happend`
  hsingleton (Phantom 84) `happend`
  hsingleton (Phantom 85) `happend`
  hsingleton (Phantom 86) `happend`
  hsingleton (Phantom 87) `happend`
  hsingleton (Phantom 88) `happend`
  hsingleton (Phantom 89) `happend`
  hsingleton (Phantom 90) `happend`
  hsingleton (Phantom 91) `happend`
  hsingleton (Phantom 92) `happend`
  hsingleton (Phantom 93) `happend`
  hsingleton (Phantom 94) `happend`
  hsingleton (Phantom 95) `happend`
  hsingleton (Phantom 96) `happend`
  hsingleton (Phantom 97) `happend`
  hsingleton (Phantom 98) `happend`
  hsingleton (Phantom 99) `happend`
  hnil
