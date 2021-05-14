{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
#endif
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- | Constant-time field accessors for extensible records. The
-- trade-off is the usual lists vs arrays one: it is fast to add an
-- element to the head of a list, but element access is linear time;
-- array access time is uniform, but extending the array is more
-- slower.
module Data.Vinyl.ARec.Internal
  -- ( ARec (..)
  -- , IndexableField
  -- , toARec
  -- , fromARec
  -- , unsafeAget
  -- , unsafeAput
  -- , alens
  -- , arecGetSubset
  -- , arecSetSubset
  -- , arecRepsMatchCoercion
  -- , arecConsMatchCoercion
  -- )
  where
import Data.Vinyl.Core
import Data.Vinyl.Lens        (RecElem(..), RecSubset(..))
import Data.Vinyl.TypeLevel
import GHC.ST

import GHC.Exts               (Any)
import Unsafe.Coerce
#if __GLASGOW_HASKELL__ < 806
import Data.Constraint.Forall (Forall)
#endif
import Data.Coerce            (Coercible)
import Data.Type.Coercion     (Coercion (..))

import GHC.Prim
import GHC.Types

-- | An array-backed extensible record with constant-time field
-- access.
data ARec (f :: k -> *) (ts :: [k]) = ARec {-# UNPACK #-} !(SmallArray# Any)
type role ARec representational nominal

-- | Get the ith element from the ARec
unsafeIxARec
  :: forall a k (f :: k -> *) (ts :: [k]).
     ARec f ts
  -> Int
  -> a
unsafeIxARec (ARec arr) (I# ix#) =
  case indexSmallArray# arr ix# of
    (# v #) -> unsafeCoerce v
{-# INLINE unsafeIxARec #-}

-- | Given that @xs@ and @ys@ have the same length, and mapping
-- @f@ over @xs@ and @g@ over @ys@ produces lists whose elements
-- are pairwise 'Coercible', @ARec f xs@ and @ARec g ys@ are
-- 'Coercible'.
arecRepsMatchCoercion :: AllRepsMatch f xs g ys => Coercion (ARec f xs) (ARec g ys)
arecRepsMatchCoercion = unsafeCoerce (Coercion :: Coercion () ())

-- | Given that @forall x. Coercible (f x) (g x)@, produce a coercion from
-- @ARec f xs@ to @ARec g xs@. While the constraint looks a lot like
-- @Coercible f g@, it is actually weaker.

#if __GLASGOW_HASKELL__ >= 806
arecConsMatchCoercion ::
  (forall (x :: k). Coercible (f x) (g x)) => Coercion (ARec f xs) (ARec g xs)
arecConsMatchCoercion = unsafeCoerce (Coercion :: Coercion () ())
#else
arecConsMatchCoercion :: forall k (f :: k -> *) (g :: k -> *) (xs :: [k]).
  Forall (Similar f g) => Coercion (Rec f xs) (Rec g xs)
-- Why do we need this? No idea, really. I guess some change in
-- newtype handling for Coercible in 8.6?
arecConsMatchCoercion = unsafeCoerce (Coercion :: Coercion (Rec f xs) (Rec f xs))
#endif

{-
-- This is sensible, but the ergonomics are likely quite bad thanks to the
-- interaction between Coercible resolution and resolution in the presence of
-- quantified constraints. Is there a good way to do this?

arecConsMatchCoercible :: forall k f g rep (r :: TYPE rep).
     (forall (x :: k). Coercible (f x) (g x))
  => ((forall (xs :: [k]). Coercible (ARec f xs) (ARec g xs)) => r) -> r
arecConsMatchCoercible f = f
-}

-- | Convert a 'Rec' into an 'ARec' for constant-time field access.
toARec :: forall f ts. (NatToInt (RLength ts)) => Rec f ts -> ARec f ts
toARec r =
  let n = natToInt @(RLength ts)
      !(I# n#) = n
  in runST $
    ST $ \st0 -> case newSmallArray# n# undefined st0 of
      (# st1 , mArr #) ->
        case go mArr st1 0 r of
          st2 ->
            case unsafeFreezeSmallArray# mArr st2 of
             (# st3, filledArray #) -> (# st3, ARec filledArray #)
  where
    go :: forall s us. SmallMutableArray# s Any
       -> State# s
       -> Int
       -> Rec f us
       -> State# s
    go _mArr stn1 n RNil = stn1
    go mArr stn1 n@(I# n#) ( x :& xs) =
      case writeSmallArray# mArr n# (unsafeCoerce x) stn1 of
        stn2 -> go mArr stn2 (n+1) xs
{-# INLINE toARec #-}

-- Don't export constructor
newtype MkARec f us =
  -- Takes the current index and returns the maximum index and an action that
  -- fills the array
  MkARec (forall s. Int -> SmallMutableArray# s Any -> ST s ())

infixr 1 &:
(&:) :: f u -> MkARec f us -> MkARec f ( u ': us )
(&:) !v (MkARec fvs) = MkARec $ \i@(I# ix) mArr ->
  let setRemainingFields = fvs (i+1)
  in do
    ST $ \st0 -> case writeSmallArray# mArr ix (unsafeCoerce v) st0 of
      st1 -> (# st1, () #)
    setRemainingFields mArr
{-# INLINE (&:) #-}

arnil :: MkARec f '[]
arnil = MkARec $ \_i _arr -> return ()
{-# INLINE arnil #-}

arec
  :: forall k (us :: [k] ) f
  . (NatToInt (RLength us)) =>
      MkARec f us
  -> ARec f us
arec (MkARec fillArray) =
  let !(I# len#) = natToInt @(RLength us)
  in runST $
     ST $ \st0 -> case newSmallArray# len# undefined st0 of
      (# st1 , mArr #) ->
        case fillArray 0 mArr of
          ST s ->
            case s st1 of
              (# st2, () #) ->
                case unsafeFreezeSmallArray# mArr st2 of
                  (# st3, filledArray #) -> (# st3, ARec filledArray #)
{-# INLINE arec #-}


class ToARec (us :: [k]) where
  aRecValues :: Rec f us -> MkARec f us

instance ToARec '[] where
  aRecValues RNil = arnil
  {-# INLINE aRecValues #-}

instance ToARec us => ToARec (u ': us) where
  aRecValues (x :& xs) = x &: aRecValues xs
  {-# INLINE aRecValues #-}

-- | Convert a 'Rec' into an 'ARec' for constant-time field access.
toARecFast
  :: forall f ts.
     (NatToInt (RLength ts), ToARec ts)
  => Rec f ts
  -> ARec f ts
toARecFast rs = arec (aRecValues rs)
{-# INLINE toARecFast #-}

-- | Defines a constraint that lets us index into an 'ARec' in order
-- to produce a 'Rec' using 'fromARec'.
class (NatToInt (RIndex t ts)) => IndexableField ts t where
instance (NatToInt (RIndex t ts)) => IndexableField ts t where

-- | Convert an 'ARec' into a 'Rec'.
fromARec :: forall f ts.
            (RecApplicative ts, RPureConstrained (IndexableField ts) ts)
         => ARec f ts -> Rec f ts
fromARec arec = rpureConstrained @(IndexableField ts) aux
  where aux :: forall t. NatToInt (RIndex t ts) => f t
        aux = unsafeIxARec arec (natToInt @(RIndex t ts))
{-# INLINE fromARec #-}

-- | Get a field from an 'ARec'.
aget :: forall t f ts. (NatToInt (RIndex t ts)) => ARec f ts -> f t
aget arec = unsafeIxARec arec (natToInt @(RIndex t ts))
{-# INLINE aget #-}

-- | Set a field in an 'ARec'.
unsafeAput :: forall t t' f ts ts'. (NatToInt (RIndex t ts))
      => f t' -> ARec f ts -> ARec f ts'
unsafeAput x (ARec arr) =
  let !(I# z#) = 0
      !(I# n#) = natToInt @(RIndex t ts)
  in runST $ ST $ \st0 ->
    case thawSmallArray# arr z# (sizeofSmallArray# arr) st0 of
     (# st1, mArr #) ->
      case writeSmallArray# mArr n# (unsafeCoerce x) st1 of
        st2 -> case unsafeFreezeSmallArray# mArr st2 of
          (# st3, arr' #) -> (# st3, ARec arr' #)
{-# INLINE unsafeAput #-}

-- | Define a lens for a field of an 'ARec'.
unsafeAlens :: forall f g t t' ts ts'. (Functor g, NatToInt (RIndex t ts))
      => (f t -> g (f t')) -> ARec f ts -> g (ARec f ts')
unsafeAlens f ar = fmap (flip (unsafeAput @t) ar) (f (aget ar))
{-# INLINE unsafeAlens #-}

-- instance (i ~ RIndex t ts, i ~ RIndex t' ts', NatToInt (RIndex t ts)) => RecElem ARec t t' ts ts' i where
--   rlens = alens
--   rget = aget
--   rput = aput

instance RecElem ARec t t' (t ': ts) (t' ': ts) 'Z where
  rlensC = unsafeAlens
  {-# INLINE rlensC #-}
  rgetC = aget
  {-# INLINE rgetC #-}
  rputC = unsafeAput @t
  {-# INLINE rputC #-}

instance (RIndex t (s ': ts) ~ 'S i, NatToInt i,  RecElem ARec t t' ts ts' i)
  => RecElem ARec t t' (s ': ts) (s ': ts') ('S i) where
  rlensC = unsafeAlens
  {-# INLINE rlensC #-}
  rgetC = aget
  {-# INLINE rgetC #-}
  rputC = unsafeAput @t
  {-# INLINE rputC #-}

-- | Get a subset of a record's fields.
arecGetSubset :: forall rs ss f.
                 (IndexWitnesses (RImage rs ss), NatToInt (RLength rs))
              => ARec f ss -> ARec f rs
arecGetSubset (ARec arr) = runST $ ST $ \st0 ->
  case newSmallArray# n# undefined st0 of
    (# st1 , mArr #) ->
      case go mArr 0 st1 (indexWitnesses @(RImage rs ss)) of
        st2 ->
          case unsafeFreezeSmallArray# mArr st2 of
            (# st3, filledArray #) -> (# st3, ARec filledArray #)
  where go :: forall s. SmallMutableArray# s Any -> Int -> State# s -> [Int] -> State# s
        go _mArr _i stn [] = stn
        go mArr to@(I# to#) stn (I# from# : is) =
          case indexSmallArray# arr from# of
            (# v #) -> case writeSmallArray# mArr to# v stn of
              stn' -> go mArr (to+1) stn' is
        !(I# n#) = natToInt @(RLength rs)
{-# INLINE arecGetSubset #-}

-- | Set a subset of a larger record's fields to all of the fields of
-- a smaller record.
arecSetSubset :: forall rs ss f. (IndexWitnesses (RImage rs ss))
              => ARec f ss -> ARec f rs -> ARec f ss
arecSetSubset (ARec arrBig) arecSmall =
  runST $ ST $ \st0 ->
  case thawSmallArray# arrBig z# (sizeofSmallArray# arrBig) st0 of
    (# st1, mArr #) -> case go mArr 0 (indexWitnesses @(RImage rs ss)) st1 of
      st2 -> case unsafeFreezeSmallArray# mArr st2 of
          (# st3, arr' #) -> (# st3, ARec arr' #)
    where
  !(I# z#) = 0
  go :: SmallMutableArray# s Any -> Int -> [Int] -> State# s -> State# s
  go _mArr _ [] stn = stn
  go mArr n (I# i# : is) stn =
    case writeSmallArray# mArr i# (unsafeIxARec arecSmall n) stn of
      stn' -> go mArr (n+1) is stn'

{-# INLINE arecSetSubset #-}

instance (is ~ RImage rs ss, IndexWitnesses is, NatToInt (RLength rs))
         => RecSubset ARec rs ss is where
  rsubsetC f big = fmap (arecSetSubset big) (f (arecGetSubset big))
  {-# INLINE rsubsetC #-}

instance (RPureConstrained (IndexableField rs) rs,
          RecApplicative rs,
          Show (Rec f rs)) => Show (ARec f rs) where
  show = show . fromARec

instance (RPureConstrained (IndexableField rs) rs,
          RecApplicative rs,
          Eq (Rec f rs)) => Eq (ARec f rs) where
  x == y = fromARec x == fromARec y

instance (RPureConstrained (IndexableField rs) rs,
          RecApplicative rs,
          Ord (Rec f rs)) => Ord (ARec f rs) where
  compare x y = compare (fromARec x) (fromARec y)
