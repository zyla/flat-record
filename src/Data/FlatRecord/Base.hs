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
{-# LANGUAGE TypeInType #-} -- for 'Field' type alias
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.FlatRecord.Base where

import Data.Proxy
import GHC.Base (Type)
import GHC.TypeLits
import Data.Functor.Identity
import Data.FlatHList
import Data.FlatHList.Internal (Index(Index))
import GHC.OverloadedLabels

type Field = Type

newtype (label :: Symbol) :-> (a :: Type) = Val { unVal :: a }
  deriving (Eq, Enum, Bounded, Monoid)

instance (KnownSymbol label, Show a) => Show (label :-> a) where
  show (Val x) = labelVal (Label @label) ++ " :-> " ++ show x

newtype Record xs = Record { unRecord :: HList xs }

deriving instance All Eq xs => Eq (Record xs)
deriving instance All Monoid xs => Monoid (Record xs)

instance All Show xs => Show (Record xs) where
  show (Record xs) = show xs

(=:) :: forall label a. Label label -> a -> Record '[label :-> a]
Label =: value = Record (hsingleton (Val value))

-- | Paste two records together.
(<+>) :: Record xs -> Record ys -> Record (xs ++ ys)
Record xs <+> Record ys = Record (xs `happend` ys)

infixr 5 <+>

rnil :: Record '[]
rnil = Record hnil

rcons :: x -> Record xs -> Record ((label :-> x) : xs)
rcons x (Record xs) = Record (Val x `hcons` xs)

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

rindex :: RIndex rs label a -> Record rs -> a
rindex (RIndex i) (Record xs) = unVal (hindex i xs)

rcast :: Subset ys xs => Record xs -> Record ys
rcast (Record xs) = Record (hcast xs)

newtype RIndex rs label a = RIndex { unRIndex :: Index rs (label :-> a) }

class LiftC c field where
  liftC :: (forall label a. (field ~ (label :-> a), KnownSymbol label, c a) =>
            Proxy (label :-> a) -> r) -> r

instance (KnownSymbol label, c a) => LiftC c (label :-> a) where
  liftC f = f Proxy

type RAll c = All (LiftC c)

rcgenerate :: forall c rs. RAll c rs => (forall label a. c a => RIndex rs label a -> a) -> Record rs
rcgenerate f = Record $ hcgenerate @(LiftC c)
  $ \(index :: Index rs field) ->
    liftC @c @field $ \(_ :: Proxy (label :-> a)) ->
      Val $ f @label @a (RIndex index)

class LiftC2 c field_a field_b where
  liftC2 :: (forall label a b.
              ( field_a ~ (label :-> a)
              , field_b ~ (label :-> b)
              , KnownSymbol label
              , c a b )
            => Proxy (label :-> a)
            -> Proxy (label :-> b)
            -> r)
        -> r

instance (KnownSymbol label, c a b) => LiftC2 c (label :-> a) (label :-> b) where
  liftC2 f = f Proxy Proxy

type RAll2 c = All2 (LiftC2 c)

rcgenerate2 :: forall c rs ss.
     RAll2 c rs ss
  => (forall label a b. c a b => RIndex rs label a -> RIndex ss label b -> b)
  -> Record ss
rcgenerate2 f = Record $ hcgenerate2 @(LiftC2 c)
  $ \(xindex :: Index rs xfield) (yindex :: Index ss yfield) ->
    liftC2 @c @xfield @yfield
      $ \(_ :: Proxy (label :-> a)) (_ :: Proxy (label :-> b)) ->
        Val $ f @label @a @b (RIndex xindex) (RIndex yindex)

class RCGenerate2A c rs ss where
  rcgenerate2A ::
       Applicative f
    => (forall label a b. c a b => RIndex rs label a -> RIndex ss label b -> f b)
    -> f (Record ss)

instance (c x y, RCGenerate2A c xs ys)
    => RCGenerate2A c ((label :-> x) : xs) ((label :-> y) : ys) where
  rcgenerate2A f =
    rcons
      <$> f (RIndex (at @0)) (RIndex (at @0))
      <*> rcgenerate2A @c @xs @ys (\i1 i2 -> f (shiftIndex i1) (shiftIndex i2))

    where
      shiftIndex :: forall xs label field a. RIndex xs label a -> RIndex (field : xs) label a
      shiftIndex (RIndex (Index index)) = RIndex (Index (index + 1))

instance RCGenerate2A c '[] '[] where
  rcgenerate2A _ = pure rnil

rcmap :: forall c rs ss.
     RAll2 c rs ss
  => (forall a b. c a b => a -> b)
  -> Record rs
  -> Record ss
rcmap f rs = rcgenerate2 @c @rs (\i _ -> f (rindex i rs))
