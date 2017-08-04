{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.FlatRecord.Base where

import GHC.Base (Type)
import GHC.TypeLits
import Data.FlatHList
import GHC.OverloadedLabels

type Field = Type

newtype (label :: Symbol) :-> (a :: Type) = Val { unVal :: a }
  deriving (Eq, Enum, Bounded, Monoid)

instance (KnownSymbol label, Show a) => Show (label :-> a) where
  show (Val x) = labelVal (Label @label) ++ " :-> " ++ show x

newtype Record xs = Record { unRecord :: HList xs }

deriving instance All Eq xs => Eq (Record xs)
deriving instance All Show xs => Show (Record xs)
deriving instance All Monoid xs => Monoid (Record xs)

(=:) :: forall label a. Label label -> a -> Record '[label :-> a]
Label =: value = Record (hsingleton (Val value))

-- | Paste two records together.
(<+>) :: Record xs -> Record ys -> Record (xs ++ ys)
Record xs <+> Record ys = Record (xs `happend` ys)

infixr 5 <+>

rnil :: Record '[]
rnil = Record hnil

-- | A singleton for record labels.
data Label (label :: Symbol) where
  Label :: KnownSymbol label => Label label

-- Why not @IsLabel label (Label label)@? Given such an instance, GHC doesn't
-- infer the first parameter from the second. This instance is more general,
-- therefore matches "sooner".
instance (label ~ label', KnownSymbol label) => IsLabel label' (Label label) where
  fromLabel _ = Label

labelVal :: Label label -> String
labelVal label@Label = symbolVal label

type family IndexOfLabel (label :: Symbol) (xs :: [Field]) :: Nat where
  IndexOfLabel label ((label :-> a) : xs) = 0
  IndexOfLabel label ((other :-> a) : xs) = 1 + IndexOfLabel label xs

-- | @HasLabel label a rs@ - a Constraint saying that the list of record fields
-- @rs@ has a field with label @label@ and type @a@.
type HasLabel (label :: Symbol) (a :: Type) (rs :: [Field]) =
  ( KnownNat (IndexOfLabel label rs)
  , ElemAt (IndexOfLabel label rs) rs ~ (label :-> a) )

-- | @get \@label record@ 
--
-- The value of field with label @label@ in @record@.
--
-- Use with @TypeApplications@.
get :: forall label a rs. HasLabel label a rs => Record rs -> a
get = unVal . hindex (at @(IndexOfLabel label rs)) . unRecord

rcast :: Subset ys xs => Record xs -> Record ys
rcast (Record xs) = Record (hcast xs)
