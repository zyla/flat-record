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
module Data.FlatRecord where

import GHC.TypeLits
import Data.FlatHList
import GHC.OverloadedLabels

newtype (label :: Symbol) :-> (a :: *) = Val { unVal :: a }
  deriving (Eq, Enum, Bounded, Monoid)

instance (KnownSymbol label, Show a) => Show (label :-> a) where
  show (Val x) = labelVal (Label @label) ++ " :-> " ++ show x

newtype Record xs = Record (HList xs)

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
