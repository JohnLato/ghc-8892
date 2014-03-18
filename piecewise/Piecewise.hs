{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Polynomials of the form a + bx + cx² + dx³ + …

module Piecewise
    ( Piecewise (..)
    , Segment (..)
    , Scale
    ) where

import Prelude hiding (splitAt)
import Control.Monad
import Control.Monad.ST (ST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector (MVector (..), Vector (..))
import Data.AdditiveGroup

-- | A 'Segment' of a piecewise 'Polynomial', valid for /begin/ ≤ @x@
-- < @seg_end@, where /begin/ is implicit from the previous segment, if any.

data Segment poly a = Segment{- {{{ -}
    { seg_end  ::  !a
    , seg_poly :: poly a
    } deriving (Eq, Show)

newtype Piecewise poly a = Piecewise {unPiecewise :: Vector (Segment poly a)} deriving (Show, Eq)

-- something about this instance is triggering trac-8892.  Doing either
-- of the following is sufficient for compilation to succeed:
--
--  - removing the {-# INLINE negateV #-}
--  - removing {-# SPECIALISE instance AdditiveGroup (Piecewise Scale Double) #-}
instance (AdditiveGroup (poly a), Ord a) => AdditiveGroup (Piecewise poly a) where
    zeroV = Piecewise V.empty
    {-# INLINE negateV #-}
    negateV = Piecewise . V.map (\ seg@Segment {..} -> seg {seg_poly = negateV seg_poly}) . unPiecewise
    Piecewise as ^+^ Piecewise bs = Piecewise $
        if V.null as then bs else if V.null bs then as else
            V.create $ MV.new (V.length as + V.length bs - 1) >>= merge where
        iMax = V.length as - 1
        jMax = V.length bs - 1
        merge :: forall s. MVector s (Segment poly a) -> ST s (MVector s (Segment poly a))
        merge mv = go 0 0 0 where
            go :: Int -> Int -> Int -> ST s (MVector s (Segment poly a))
            go n i j = do
                MV.unsafeWrite mv n ab
                if aLast && bLast
                    then return (MV.unsafeTake (succ n) mv)
                    else go (succ n) i' j'
              where
                a = V.unsafeIndex as i
                b = V.unsafeIndex bs j
                ab = Segment end (seg_poly a ^+^ seg_poly b)
                aLast = i >= iMax
                bLast = j >= jMax
                aNext = (seg_end a, succ i, j)
                bNext = (seg_end b, i, succ j)
                -- This extrapolates the last seg_poly past seg_end.
                (end, i', j') = case seg_end a `compare` seg_end b of
                    LT -> if aLast then bNext else aNext
                    GT -> if bLast then aNext else bNext
                    EQ -> (seg_end a, succ i `min` iMax, succ j `min` jMax)
    {-# SPECIALISE instance AdditiveGroup (Piecewise Scale Double) #-}

newtype Scale a = Scale a

instance Num a => AdditiveGroup (Scale a) where
    zeroV = Scale 0
    Scale l ^+^ Scale r = Scale (l+r)
    negateV (Scale s) = Scale (-s)
