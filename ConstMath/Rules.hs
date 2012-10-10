{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

module ConstMath.Rules (
  rationalToFloat
, rationalToDouble
) where

import GHC.Float
import GHC.Real

rationalToFloat :: Rational -> Float
rationalToFloat (n:%0)
    | n == 0        = 0/0
    | n < 0         = (-1)/0
    | otherwise     = 1/0
rationalToFloat (n:%d)
    | n == 0        = encodeFloat 0 0
    | n < 0         = -(fromRat'' minEx mantDigs (-n) d)
    | otherwise     = fromRat'' minEx mantDigs n d
      where
        minEx       = fst (floatRange (0::Float))
        mantDigs    = floatDigits (0 :: Float)
{-# NOINLINE [1] rationalToFloat #-}

rationalToDouble :: Rational -> Double
rationalToDouble (n:%0)
    | n == 0        = 0/0
    | n < 0         = (-1)/0
    | otherwise     = 1/0
rationalToDouble (n:%d)
    | n == 0        = encodeFloat 0 0
    | n < 0         = -(fromRat'' minEx mantDigs (-n) d)
    | otherwise     = fromRat'' minEx mantDigs n d
      where
        minEx       = fst (floatRange (0 :: Double))
        mantDigs    = floatDigits (0 :: Double)
{-# NOINLINE [1] rationalToDouble #-}

{-# RULES
"ConstMath/rationalToFloat"  fromRational = rationalToFloat
"ConstMath/rationalToDouble" fromRational = rationalToDouble
      #-}
