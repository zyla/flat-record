{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -ddump-simpl #-}

module Main (main) where

import Data.FlatHList
import Data.FlatRecord
import SuperRecord ((:=)(..), (&))
import qualified SuperRecord as S
import Criterion.Main

main = defaultMain
  [ bench "FlatRecord"  $ whnf flatRecord 37
  , bench "SuperRecord" $ whnf superRecord 37
  ]

type FieldsFlatRec = 
  "field0" :-> Int :
  "field1" :-> Int :
  "field2" :-> Int :
  "field3" :-> Int :
  "field4" :-> Int :
  "field5" :-> Int :
  "field6" :-> Int :
  "field7" :-> Int :
  "field8" :-> Int :
  "field9" :-> Int :
  '[]

flatRecord :: Int -> Record FieldsFlatRec
flatRecord x =
  #field0 =: x <+>
  #field1 =: x <+>
  #field2 =: x <+>
  #field3 =: x <+>
  #field4 =: x <+>
  #field5 =: x <+>
  #field6 =: x <+>
  #field7 =: x <+>
  #field8 =: x <+>
  #field9 =: x <+>
  rnil
{-# NOINLINE flatRecord #-}


type FieldsSuperRec = 
  "field0" := Int :
  "field1" := Int :
  "field2" := Int :
  "field3" := Int :
  "field4" := Int :
  "field5" := Int :
  "field6" := Int :
  "field7" := Int :
  "field8" := Int :
  "field9" := Int :
  '[]

superRecord :: Int -> S.Record FieldsSuperRec
superRecord x =
  (#field0 := x) &
  (#field1 := x) &
  (#field2 := x) &
  (#field3 := x) &
  (#field4 := x) &
  (#field5 := x) &
  (#field6 := x) &
  (#field7 := x) &
  (#field8 := x) &
  (#field9 := x) &
  S.rnil
{-# NOINLINE superRecord #-}
