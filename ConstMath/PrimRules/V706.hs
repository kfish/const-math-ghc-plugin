{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

{-# OPTIONS_GHC -Wall #-}
module ConstMath.PrimRules.V706 (
  unaryPrimRules
, binaryPrimRules
) where

unaryPrimRules :: [(String, (forall a. RealFloat a => a -> a))]
unaryPrimRules =
    [ ("GHC.Prim.expDouble#"    , exp)
    , ("GHC.Prim.logDouble#"    , log)
    , ("GHC.Prim.sqrtDouble#"   , sqrt)
    , ("GHC.Prim.sinDouble#"    , sin)
    , ("GHC.Prim.cosDouble#"    , cos)
    , ("GHC.Prim.tanDouble#"    , tan)
    , ("GHC.Prim.asinDouble#"   , asin)
    , ("GHC.Prim.acosDouble#"   , acos)
    , ("GHC.Prim.atanDouble#"   , atan)
    , ("GHC.Prim.sinhDouble#"   , sinh)
    , ("GHC.Prim.coshDouble#"   , cosh)
    , ("GHC.Prim.tanhDouble#"   , tanh)

    , ("GHC.Prim.expFloat#"     , exp)
    , ("GHC.Prim.logFloat#"     , log)
    , ("GHC.Prim.sqrtFloat#"    , sqrt)
    , ("GHC.Prim.sinFloat#"     , sin)
    , ("GHC.Prim.cosFloat#"     , cos)
    , ("GHC.Prim.tanFloat#"     , tan)
    , ("GHC.Prim.asinFloat#"    , asin)
    , ("GHC.Prim.acosFloat#"    , acos)
    , ("GHC.Prim.atanFloat#"    , atan)
    , ("GHC.Prim.sinhFloat#"    , sinh)
    , ("GHC.Prim.coshFloat#"    , cosh)
    , ("GHC.Prim.tanhFloat#"    , tanh)
    ]

binaryPrimRules :: [(String, (forall a. RealFloat a => a -> a -> a))]
binaryPrimRules =
    [ ("GHC.Prim.powerFloat#"   , (**))  -- FloatPowerOp
    , ("GHC.Prim.**##"          , (**))  -- DoublePowerOp
    ]
