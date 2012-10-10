{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}

module ConstMath.Pass (
      constMathProgram
) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import GhcPlugins
import Var

constMathProgram :: [CoreBind] -> CoreM [CoreBind]
constMathProgram binds = do
    putMsgS "\nStarting ConstMath pass"
    mapM (subBind "") binds

subBind :: String -> CoreBind -> CoreM CoreBind
subBind tab bndr@(NonRec b rhs) = do
    putMsgS $ tab ++ "Non-recursive binding named " ++ showSDoc (ppr b)
    rhs' <- subExpr tab rhs
    return (NonRec b rhs')
subBind tab bndr@(Rec pairs) = do
    mapM (uncurry printRecBind) pairs
    return bndr

printRecBind b _e = do
    putMsgS $ "Recursive binding " ++ showSDoc (ppr b)

subExpr :: String -> CoreExpr -> CoreM CoreExpr

subExpr tab expr@(Type t) = do
    putMsgS $ tab ++ "Type " ++ showSDoc (ppr t)
    return expr

subExpr tab expr@(Coercion co) = do
    putMsgS $ tab ++ "Coercion"
    return expr

subExpr tab expr@(Lit lit) = do
    putMsgS $ tab ++ "Lit " ++ showSDoc (ppr lit)
    return expr

subExpr tab expr@(Var v) = do
    putMsgS $ tab ++ "Var " ++ showSDoc (ppr v)
    return expr

subExpr tab (App f a) = do
    let funcName = showSDoc (ppr f)
    putMsgS $ tab ++ "App " ++ funcName
    f' <- subExpr (tab ++ "< ") f
    a' <- subExpr (tab ++ "> ") a
    collapse (App f' a')

subExpr tab (Tick t e) = do
    putMsgS $ tab ++ "Tick"
    e' <- subExpr (tab ++ "  ") e
    return (Tick t e')

subExpr tab (Cast e co) = do
    putMsgS $ tab ++ "Cast"
    e' <- subExpr (tab ++ "  ") e
    return (Cast e' co)

subExpr tab (Lam b e) = do
    putMsgS $ tab ++ "Lam"
    e' <- subExpr (tab ++ "  ") e
    return (Lam b e')

subExpr tab (Let bind e) = do
    putMsgS $ tab ++ "Let"
    bind' <- subBind tab bind
    e' <- subExpr (tab ++ "  ") e
    return (Let bind' e')

subExpr tab expr@(Case scrut bndr ty alts) = do
    putMsgS $ tab ++ "Case"
    return expr

----------------------------------------------------------------------

collapse :: CoreExpr -> CoreM CoreExpr
collapse expr@(App f1 _)
  | Just f <- cmSubst <$> findSub f1
    = f expr
collapse expr = return expr

mkUnaryCollapseIEEE :: (forall a. RealFloat a => (a -> a))
                    -> CoreExpr
                    -> CoreM CoreExpr
mkUnaryCollapseIEEE fnE expr@(App f1 (App f2 (Lit lit)))
    | isDHash f2, MachDouble d <- lit = evalUnaryIEEE d mkDoubleLitDouble
    | isFHash f2, MachFloat d  <- lit = evalUnaryIEEE d mkFloatLitFloat
    where
        evalUnaryIEEE d mkLit = do
            let sub = fnE (fromRational d)
            maybe (return expr)
              (return . App f2 . mkLit)
              =<< maybeIEEE (fromJust $ funcName f1) sub
mkUnaryCollapseIEEE _ expr = return expr

mkUnaryCollapseNum :: (forall a . Num a => (a -> a))
                   -> CoreExpr
                   -> CoreM CoreExpr
mkUnaryCollapseNum fnE (App f1 (App f2 (Lit lit)))
    | isDHash f2, MachDouble d <- lit =
        evalUnaryNum fromRational d mkDoubleLitDouble
    | isFHash f2, MachFloat d  <- lit =
        evalUnaryNum fromRational d mkFloatLitFloat
    | isIHash f2, MachInt d    <- lit =
        evalUnaryNum fromIntegral d mkIntLitInt
    | isWHash f2, MachWord d   <- lit =
        evalUnaryNum fromIntegral d mkWordLitWord
    where
        evalUnaryNum from d mkLit = do
            let sub = fnE (from d)
            return (App f2 (mkLit sub))
mkUnaryCollapseNum _ expr = return expr

mkBinaryCollapse :: (forall a. RealFloat a => (a -> a -> a))
                 -> CoreExpr
                 -> CoreM CoreExpr
mkBinaryCollapse fnE expr@(App (App f1 (App f2 (Lit lit1))) (App f3 (Lit lit2)))
    | isDHash f2 && isDHash f3
    , MachDouble d1 <- lit1, MachDouble d2 <- lit2 =
        evalBinaryIEEE d1 d2 mkDoubleLitDouble
    | isFHash f2 && isFHash f3
    , MachFloat d1  <- lit1, MachFloat d2  <- lit2 =
        evalBinaryIEEE d1 d2 mkFloatLitFloat
    where
        evalBinaryIEEE d1 d2 mkLit = do
            let sub = fnE (fromRational d1) (fromRational d2)
            maybe (return expr) (\x -> return (App f2 (mkLit x)))
                  =<< maybeIEEE (fromJust $ funcName f1) sub
mkBinaryCollapse _ expr = return expr

fromRationalCollapse :: CoreExpr -> CoreM CoreExpr
fromRationalCollapse expr@(App f1 (App (App f2 (Lit (LitInteger n _))) (Lit (LitInteger d _))))
    | Just "ConstMath.Rules.rationalToFloat" <- funcName f1
    , Just "GHC.Real.:%" <- funcName f2
      = do
          let sub = fromRational $ (fromInteger n) / (fromInteger d)
          maybe (return expr) (\x -> return (mkFloatExpr x)) =<< maybeIEEE (fromJust $ funcName f1) sub
fromRationalCollapse expr@(App f1 (App (App f2 (Lit (LitInteger n _))) (Lit (LitInteger d _))))
    | Just "ConstMath.Rules.rationalToDouble" <- funcName f1
    , Just "GHC.Real.:%" <- funcName f2
      = do
          let sub = fromRational $ (fromInteger n) / (fromInteger d)
          maybe (return expr) (\x -> return (mkDoubleExpr x)) =<< maybeIEEE (fromJust $ funcName f1) sub
fromRationalCollapse expr = return expr

maybeIEEE :: RealFloat a => String -> a -> CoreM (Maybe a)
maybeIEEE s d
    | isNaN d = do
        err "NaN"
        return Nothing
    | isInfinite d = do
        err "infinite"
        return Nothing
    | isDenormalized d = do
        err "denormalized"
        return Nothing
    | isNegativeZero d = do
        err "negative zero"
        return Nothing
    | otherwise = do
        putMsgS $ "Result of replacing " ++ s ++ " is ok"
        return (Just d)
    where
        err v = errorMsgS $ "Skipping replacement of " ++ s ++ " result " ++ v

----------------------------------------------------------------------

data CMSub = CMSub
    { cmFuncName :: String
    , cmSubst    :: CoreExpr -> CoreM CoreExpr
    }

unarySubIEEE :: String -> (forall a. RealFloat a => a -> a) -> CMSub
unarySubIEEE nm fn = CMSub nm (mkUnaryCollapseIEEE fn)

unarySubNum :: String -> (forall a . Num a => (a -> a)) -> CMSub
unarySubNum nm fn = CMSub nm (mkUnaryCollapseNum fn)

binarySub :: String -> (forall a. RealFloat a => a -> a -> a) -> CMSub
binarySub nm fn = CMSub nm (mkBinaryCollapse fn)

funcName :: CoreExpr -> Maybe String
funcName = listToMaybe . words . showSDoc . ppr

isFHash :: CoreExpr -> Bool
isFHash = maybe False ((==) "GHC.Types.F#") . funcName

isDHash :: CoreExpr -> Bool
isDHash = maybe False ((==) "GHC.Types.D#") . funcName

isIHash :: CoreExpr -> Bool
isIHash = maybe False ((==) "GHC.Types.I#") . funcName

isWHash :: CoreExpr -> Bool
isWHash = maybe False ((==) "GHC.Word.W#") . funcName

findSub :: CoreExpr -> Maybe CMSub
findSub = flip Map.lookup subFunc <=< funcName

subs =
    [ unarySubIEEE "GHC.Float.exp"    exp
    , unarySubIEEE "GHC.Float.log"    log
    , unarySubIEEE "GHC.Float.sqrt"   sqrt
    , unarySubIEEE "GHC.Float.sin"    sin
    , unarySubIEEE "GHC.Float.cos"    cos
    , unarySubIEEE "GHC.Float.tan"    tan
    , unarySubIEEE "GHC.Float.asin"   asin
    , unarySubIEEE "GHC.Float.acos"   acos
    , unarySubIEEE "GHC.Float.atan"   atan
    , unarySubIEEE "GHC.Float.sinh"   sinh
    , unarySubIEEE "GHC.Float.cosh"   cosh
    , unarySubIEEE "GHC.Float.tanh"   tanh
    , unarySubIEEE "GHC.Float.asinh"  asinh
    , unarySubIEEE "GHC.Float.acosh"  acosh
    , unarySubIEEE "GHC.Float.atanh"  atanh
    , unarySubNum "GHC.Num.negate"   negate
    , unarySubNum "GHC.Num.abs"      abs
    , unarySubNum "GHC.Num.signum"   signum
    , CMSub    "ConstMath.Rules.rationalToFloat" fromRationalCollapse
    , CMSub    "ConstMath.Rules.rationalToDouble" fromRationalCollapse
    ]

subFunc :: Map String CMSub
subFunc = Map.fromList $ zip (map cmFuncName subs) subs
