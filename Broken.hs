module Broken
    ( evil
    ) where

import Prelude
import Data.VectorSpace
import Piecewise

------------------------------------------------------------------------

type PriceFn = Piecewise Scale Double

------------------------------------------------------------------------

evil :: PriceFn -> PriceFn
evil pFn = (zeroV ^+^ negateV pFn)

