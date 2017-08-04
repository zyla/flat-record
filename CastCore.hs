{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -ddump-simpl -O2 #-}

module Cast3 (flatCast) where

import Data.FlatHList
import Data.Proxy
import Control.DeepSeq
import Criterion.Main

flatCast :: HList Fields -> HList Fields2
flatCast = hcast

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
  Phantom 12 :
  Phantom 13 :
  Phantom 14 :
  '[]

type Fields2 = 
  Phantom 14 :
  Phantom 13 :
  Phantom 12 :
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
