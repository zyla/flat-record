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
import GHC.TypeLits
import GHC.Base (Type)
import GHC.Exts (Any, Constraint)
import Unsafe.Coerce (unsafeCoerce)
import Data.FlatHList.TypeLevel

-- INVARIANT (in pseudo-dependent Haskell):
--   if @HL vector :: HList xs@
--   then @length vector = Length xs@
--   and @forall i. (0 <= i < length vector) => (vector ! i) :: ElemAt i xs@
--
--   In other words, types in @vector@ match @xs@.
newtype HList (xs :: [Type]) = HL (V.Vector Any)

-- | @index \@i vector@ - Get ith element from the HList.
hindex :: Index xs a -> HList xs -> a
hindex (Index i) (HL vector) =
  unsafeFromAny (vector ! i)
  -- Safety proof:
  --
  -- Theorem: @(vector ! i) :: a@
  --
  -- TODO
{-# INLINE hindex #-}

-- | @Index i :: Index xs a@
--
-- An index into the type level list of types @xs@,
-- also representing the proof that @ElemAt i xs ~ a@.
--
-- Can be used to access @i@th element at type @a@ of any vector of shape @xs@.
newtype Index (xs :: [k]) (a :: k) = Index { indexValue :: Int }

at :: forall (i :: Nat) xs. KnownNat i => Index xs (ElemAt i xs)
at = Index (reifyNat @i)
  -- TODO: Safety proof

hset :: forall i b xs. KnownNat i => b -> HList xs -> HList (SetAt i b xs)
hset b (HL vector) = HL $
  V.modify (\mvector -> VM.write mvector (reifyNat @i) (unsafeToAny b)) vector
  -- TODO: Safety proof
{-# INLINE hset #-}

hget :: forall a i xs. (i ~ IndexOf a xs, ElemAt i xs ~ a, KnownNat i) => HList xs -> a
hget = hindex (at @i)
{-# INLINE hget #-}

reifyNat :: forall i. KnownNat i => Int
reifyNat = fromIntegral (natVal (Proxy @i))

hnil :: HList '[]
hnil = HL V.empty

huncons :: HList (x : xs) -> (x, HList xs)
huncons xs@(HL vector) = (hindex (at @0) xs, HL (V.tail vector))
  -- TODO: Safety proof

hsingleton :: a -> HList '[a]
hsingleton x =
  HL (V.singleton (unsafeToAny x))
  -- Safety proof:
  --
  -- Theorem (invariant for @HList '[a]@):
  --   @forall i. (0 <= i < length (V.singleton x)) =>
  --      (V.singleton x ! i) :: ElemAt i '[a]@
  --
  -- There is only one valid index: 0.
  -- @(V.singleton x ! 0) = x@  -- from properties of singleton
  -- @a ~ ElemAt 0 '[a]@         -- trivial
  -- therefore
  -- @(V.singleton x ! 0) :: ElemAt 0 '[a]@
{-# INLINE hsingleton #-}

happend :: HList xs -> HList ys -> HList (xs ++ ys)
happend (HL v1) (HL v2) =
  HL (v1 <> v2)
  -- Safety proof:
  --
  -- Theorem (invariant for @HList (xs ++ ys)@):
  --   @forall i. (0 <= i < length (v1 <> v2)) =>
  --      ((v1 <> v2) ! i) :: ElemAt i (xs ++ ys)@
  --
  -- Case 1: i < length v1
  --   @(v1 <> v2) ! i     = v1 ! i@      -- from properties of (<>)
  --   @ElemAt i (xs ++ ys) ~ ElemAt i xs@  -- from properties of (++)
  --
  --   It remains to prove
  --   @(v1 ! i) :: ElemAt i xs@
  --   which follows from invariant for @HList xs@.
  --
  -- Case 1: i >= length v1
  --   @(v1 <> v2) ! i     = v2 ! (i - length v1)@
  --       -- from properties of (<>)
  --   @ElemAt i (xs ++ ys) ~ ElemAt (i - Length xs) ys@
  --       -- from properties of (++)
  --
  --   It remains to prove
  --   @(v2 ! (i - length v1)) :: ElemAt (i - length v1) ys@
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

hcgenerate :: forall (c :: Type -> Constraint) xs.
     All c xs
  => (forall a. c a => Index xs a -> a)
  -> HList xs
hcgenerate f = HL $ runST $ do
  mvector <- VM.new (reifyNat @(Length xs))
  _ :: [()] <- ctraverse_off @c @xs 0 $ \index ->
    VM.write mvector (indexValue index) (unsafeToAny (f index))
  V.unsafeFreeze mvector
  -- TODO: Safety proof
{-# INLINE hcgenerate #-}

class KnownNat (Length xs) => All2 (c :: Type -> Type -> Constraint) xs ys where
  ctraverse2_off :: forall f r.
       Applicative f
    => Int -- ^ offset
    -> (forall a b. c a b => Index xs a -> Index ys b -> f r)
    -> f [r]

instance All2 c '[] '[] where
  ctraverse2_off _ _ = pure []
  {-# INLINE ctraverse2_off #-}

instance (c x y, All2 c xs ys, KnownNat (1 + Length xs)) => All2 c (x : xs) (y : ys) where
  ctraverse2_off off f =
    (:) <$> f @x @y (Index off) (Index off)
        <*> ctraverse2_off @c @xs @ys (off + 1) (shiftIndex f)
                                              -- TODO: Safety proof
    where
      shiftIndex :: forall a1 b1 r. c a1 b1 =>
                    (forall a b. c a b => Index (x : xs) a -> Index (y : ys) b -> r)
                 -> Index xs a1 -> Index ys b1 -> r
      shiftIndex f (Index xindex) (Index yindex) = f @a1 @b1 (Index xindex) (Index yindex)
  {-# INLINE ctraverse2_off #-}

hcgenerate2 :: forall (c :: Type -> Type -> Constraint) xs ys.
   ( All2 c xs ys )
  => (forall a b. c a b => Index xs a -> Index ys b -> b)
  -> HList ys
hcgenerate2 f = HL $ runST $ do
  mvector <- VM.new (reifyNat @(Length xs))
  _ :: [()] <- ctraverse2_off @c @xs @ys 0 $ \xindex yindex ->
    VM.write mvector (indexValue yindex) (unsafeToAny (f xindex yindex))
  V.unsafeFreeze mvector
  -- TODO: Safety proof
{-# INLINE hcgenerate2 #-}

hcpure :: forall (c :: Type -> Constraint) xs.
   ( All c xs )
  => (forall a. c a => a)
  -> HList xs
hcpure value = hcgenerate @c (\_ -> value)

class KnownNat (Length xs) => All (c :: Type -> Constraint) xs where
  ctraverse_off :: forall f r.
       Applicative f
    => Int -- ^ offset
    -> (forall a. c a => Index xs a -> f r)
    -> f [r]

instance All c '[] where
  ctraverse_off _ _ = pure []
  {-# INLINE ctraverse_off #-}

instance (c x, All c xs, KnownNat (1 + Length xs)) => All c (x : xs) where
  ctraverse_off off f =
    (:) <$> f @x (Index off)
        <*> ctraverse_off @c @xs (off + 1) (shiftIndex f)
                                              -- TODO: Safety proof
    where
      shiftIndex :: forall b r. c b =>
                    (forall a. c a => Index (x : xs) a -> r)
                 -> Index xs b -> r
      shiftIndex f (Index index) = f @b (Index index)
  {-# INLINE ctraverse_off #-}

class IsF f x where
  prf_isF :: (forall a. f a) -> x

instance x ~ f a => IsF f x where
  prf_isF = id

hcToList :: forall (c :: Type -> Constraint) xs r.
     All c xs
  => (forall a. c a => a -> r)
  -> HList xs
  -> [r]
hcToList f xs =
  runIdentity $ ctraverse_off @c @xs 0 $ \index ->
    pure $ f (hindex index xs)
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
  -> HList (ElemsAt is xs)
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

type Subset ys xs =
   ( ElemsAt (IndexesOf ys xs) xs ~ ys
   , ReifyNats (IndexesOf ys xs)
   , KnownNat (Length (IndexesOf ys xs)) )

hcast :: forall xs ys.
     Subset ys xs
  => HList xs
  -> HList ys
hcast = hindexes @(IndexesOf ys xs)
{-# INLINE hcast #-}

hcToList2 :: forall (c :: Type -> Constraint) xs r.
     All c xs
  => (forall a. c a => a -> a -> r)
  -> HList xs
  -> HList xs
  -> [r]
hcToList2 f xs ys =
  runIdentity $ ctraverse_off @c @xs 0 $ \index ->
    pure $ f (hindex index xs) (hindex index ys)
  -- TODO: Safety proof

hczipWith :: forall (c :: Type -> Constraint) xs.
     All c xs
  => (forall a. c a => a -> a -> a)
  -> HList xs
  -> HList xs
  -> HList xs
hczipWith f xs ys = 
  hcgenerate @c $ \index ->
    f (hindex index xs) (hindex index ys)
{-# INLINE hczipWith #-}

-- | @ElemAt i xs@ ith element of xs.
--
-- Stuck if i is out of bounds.
type family ElemAt index xs where
  ElemAt 0 (x : xs) = x
  ElemAt n (x : xs) = ElemAt (n - 1) xs

type family ElemsAt (is :: [Nat]) (xs :: [k]) :: [k] where
  ElemsAt '[] xs = '[]
  ElemsAt (i : is) xs = ElemAt i xs : ElemsAt is xs

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

hcons :: x -> HList xs -> HList (x : xs)
hcons x xs = hsingleton x `happend` xs

infixr 5 `hcons`
