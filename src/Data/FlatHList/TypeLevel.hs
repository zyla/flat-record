{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.FlatHList.TypeLevel where

import GHC.TypeLits

-- | Standard type-level boilerplate.
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x : xs) ++ ys = x : (xs ++ ys)

-- | @SetAt i x xs@ replaces ith element of xs with x.
-- Stuck if i is out of bounds.
type family SetAt (index :: Nat) (x :: k) (xs :: [k]) where
  SetAt 0 x (y : xs) = x : xs
  SetAt n x (y : xs) = y : SetAt (n - 1) x xs
