{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies,
   TypeApplications, UndecidableInstances, ScopedTypeVariables, RankNTypes,
   AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
   FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}

module Data.FlatHList.Internal where

import Data.Monoid ((<>))
import Data.Vector ((!))
import Data.List (intercalate, (!!))
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Functor.Identity
import Control.Monad.ST
import Data.Proxy
import GHC.Types
import GHC.TypeLits
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.FlatHList.TypeLevel

-- INVARIANT (in pseudo-dependent Haskell):
--   if @HL vector :: HList xs@
--   then @length vector = Length xs@
--   and @forall i. (0 <= i < length vector) => (vector ! i) :: Index i xs@
--
--   In other words, types in @vector@ match @xs@.
newtype HList (xs :: [*]) = HL (V.Vector Any)

-- | @index \@i vector@ - Get ith element from the HList.
hindex :: forall (i :: Nat) xs. KnownNat i => HList xs -> Index i xs
hindex (HL vector) =
  unsafeFromAny (vector ! reifyNat @i)
  -- Safety proof:
  --
  -- Theorem: @(vector ! i) :: Index i xs@
  --
  -- Follows directly from invariant for @HList xs@.
{-# INLINE hindex #-}

hget :: forall a i xs. (i ~ IndexOf a xs, Index i xs ~ a, KnownNat i) => HList xs -> a
hget = hindex @i
{-# INLINE hget #-}

reifyNat :: forall i. KnownNat i => Int
reifyNat = fromIntegral (natVal (Proxy @i))

hnil :: HList '[]
hnil = HL V.empty

hsingleton :: a -> HList '[a]
hsingleton x =
  HL (V.singleton (unsafeToAny x))
  -- Safety proof:
  --
  -- Theorem (invariant for @HList '[a]@):
  --   @forall i. (0 <= i < length (V.singleton x)) =>
  --      (V.singleton x ! i) :: Index i '[a]@
  --
  -- There is only one valid index: 0.
  -- @(V.singleton x ! 0) = x@  -- from properties of singleton
  -- @a ~ Index 0 '[a]@         -- trivial
  -- therefore
  -- @(V.singleton x ! 0) :: Index 0 '[a]@
{-# INLINE hsingleton #-}

happend :: HList xs -> HList ys -> HList (xs ++ ys)
happend (HL v1) (HL v2) =
  HL (v1 <> v2)
  -- Safety proof:
  --
  -- Theorem (invariant for @HList (xs ++ ys)@):
  --   @forall i. (0 <= i < length (v1 <> v2)) =>
  --      ((v1 <> v2) ! i) :: Index i (xs ++ ys)@
  --
  -- Case 1: i < length v1
  --   @(v1 <> v2) ! i     = v1 ! i@      -- from properties of (<>)
  --   @Index i (xs ++ ys) ~ Index i xs@  -- from properties of (++)
  --
  --   It remains to prove
  --   @(v1 ! i) :: Index i xs@
  --   which follows from invariant for @HList xs@.
  --
  -- Case 1: i >= length v1
  --   @(v1 <> v2) ! i     = v2 ! (i - length v1)@
  --       -- from properties of (<>)
  --   @Index i (xs ++ ys) ~ Index (i - Length xs) ys@
  --       -- from properties of (++)
  --
  --   It remains to prove
  --   @(v2 ! (i - length v1)) :: Index (i - length v1) ys@
  --   Since @i - length v1 < length v2@,
  --   this follows from invariant for @HList ys@.
  --
{-# INLINE happend #-}

unsafeFromAny :: Any -> a
unsafeFromAny = unsafeCoerce
{-# INLINE [0] unsafeFromAny #-}

unsafeToAny :: a -> Any
unsafeToAny = unsafeCoerce
{-# INLINE [0] unsafeToAny #-}

{-# RULES "unsafeFromAny/unsafeFromAny" forall x. unsafeFromAny (unsafeToAny x) = x #-}

hcpure :: forall (c :: * -> Constraint) xs.
   ( All c xs )
  => (forall a. c a => a)
  -> HList xs
hcpure value = HL $ runST $ do
  mvector <- VM.new (reifyNat @(Length xs))
  _ :: [()] <- ctraverse_off @c @xs 0 (\(_ :: Proxy (a :: *)) index ->
    VM.write mvector index (unsafeToAny (value :: a)))
  V.unsafeFreeze mvector
  -- TODO: Safety proof

class KnownNat (Length xs) => All (c :: * -> Constraint) xs where
  ctraverse_off :: forall f r.
       Applicative f
    => Int -- ^ offset
    -> (forall a. c a => Proxy a -> Int -> f r)
    -> f [r]

instance All c '[] where
  ctraverse_off _ _ = pure []
  {-# INLINE ctraverse_off #-}

instance (c x, All c xs, KnownNat (1 + Length xs)) => All c (x : xs) where
  ctraverse_off off f = (:) <$> f (Proxy @x) off <*> ctraverse_off @c @xs (off + 1) f
  {-# INLINE ctraverse_off #-}

class IsF f x where
  prf_isF :: (forall a. f a) -> x

instance x ~ f a => IsF f x where
  prf_isF = id

hcToList :: forall (c :: * -> Constraint) xs r.
     All c xs
  => (forall a. c a => a -> r)
  -> HList xs
  -> [r]
hcToList f (HL vector) =
  runIdentity $ ctraverse_off @c @xs 0
    (\(_ :: Proxy a) index -> Identity (f @a (unsafeFromAny (vector ! index))))
  -- TODO: Safety proof

instance All Show xs => Show (HList xs) where
  show xs = "[" ++ intercalate ", " (hcToList @Show show xs) ++ "]"

instance All Eq xs => Eq (HList xs) where
  xs == ys = all id (hcToList2 @Eq (==) xs ys)

instance All Monoid xs => Monoid (HList xs) where
  mempty = hcpure @Monoid mempty
  mappend = hczipWith @Monoid mappend

instance All NFData xs => NFData (HList xs) where
  rnf = rnf . hcToList @NFData rnf

hindexes :: forall (is :: [Nat]) xs.
   ( ReifyNats is, KnownNat (Length is) )
  => HList xs
  -> HList (Indexes is xs)
hindexes (HL source) = HL $ runST $ do
  target <- VM.new (reifyNat @(Length is))
  traverseNatsI_ @is 0 $ \targetIndex sourceIndex ->
    VM.write target targetIndex (V.unsafeIndex source sourceIndex)
  V.unsafeFreeze target
  -- TODO: Safety proof
{-# INLINE hindexes #-}

hseq :: HList xs -> HList xs
hseq (HL vector) = seqList (V.toList vector) `seq` HL vector
  -- TODO: Safety proof
{-# INLINE hseq #-}

seqList :: [a] -> [a]
seqList [] = []
seqList (x : xs) = let rest = seqList xs in x `seq` rest `seq` (x : rest)

hcast :: forall (is :: [Nat]) xs ys.
   ( is ~ IndexesOf xs ys, Indexes is xs ~ ys, ReifyNats is, KnownNat (Length is) )
  => HList xs
  -> HList ys
hcast = hindexes @is
{-# INLINE hcast #-}

hcToList2 :: forall (c :: * -> Constraint) xs r.
     All c xs
  => (forall a. c a => a -> a -> r)
  -> HList xs
  -> HList xs
  -> [r]
hcToList2 f (HL v1) (HL v2) =
  runIdentity $ ctraverse_off @c @xs 0 $
    \(_ :: Proxy a) index ->
      Identity $
        f @a (unsafeFromAny (v1 ! index))
             (unsafeFromAny (v2 ! index))
  -- TODO: Safety proof

hczipWith :: forall (c :: * -> Constraint) xs.
     All c xs
  => (forall a. c a => a -> a -> a)
  -> HList xs
  -> HList xs
  -> HList xs
hczipWith f (HL v1) (HL v2) = HL $ runST $ do
  mvector <- VM.new (reifyNat @(Length xs))
  _ :: [()] <- ctraverse_off @c @xs 0 $ \(_ :: Proxy a) index ->
    VM.write mvector index $
      unsafeToAny $
        f @a (unsafeFromAny (v1 ! index))
             (unsafeFromAny (v2 ! index))
  V.unsafeFreeze mvector
  -- TODO: Safety proof
{-# INLINE hczipWith #-}

-- | @Index i xs@ ith element of xs.
--
-- Stuck if i is out of bounds.
type family Index index xs where
  Index 0 (x : xs) = x
  Index n (x : xs) = Index (n - 1) xs

type family Indexes (is :: [Nat]) (xs :: [*]) :: [*] where
  Indexes '[] xs = '[]
  Indexes (i : is) xs = Index i xs : Indexes is xs

type family IndexOf a xs where
  IndexOf a (a : xs) = 0
  IndexOf a (x : xs) = 1 + IndexOf a xs

type family IndexesOf xs ys where
  IndexesOf '[] ys = '[]
  IndexesOf (x : xs) ys = IndexOf x ys : IndexesOf xs ys

type family Length xs where
  Length '[] = 0
  Length (x : xs) = 1 + Length xs

class ReifyNats (is :: [Nat]) where
  traverseNatsI_ :: Applicative m => Int -> (Int -> Int -> m ()) -> m ()

instance ReifyNats '[] where
  traverseNatsI_ _ _ = pure ()
  {-# INLINE traverseNatsI_ #-}

instance (KnownNat x, ReifyNats xs) => ReifyNats (x : xs) where
  traverseNatsI_ index f = f index (reifyNat @x) *> traverseNatsI_ @xs (index + 1) f
  {-# INLINE traverseNatsI_ #-}
