{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall #-}
module ConstMath.PrimRules (
  unaryPrimRules
, binaryPrimRules
) where

#if __GLASGOW_HASKELL__ == 704
import ConstMath.PrimRules.V704 (unaryPrimRules, binaryPrimRules)
#elif __GLASGOW_HASKELL__ >= 706
import ConstMath.PrimRules.V706 (unaryPrimRules, binaryPrimRules)
#else
#error Unsupported GHC version in PrimRules.hs!
#endif

