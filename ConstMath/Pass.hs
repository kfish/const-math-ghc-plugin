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
    mapM subBind binds

subBind :: CoreBind -> CoreM CoreBind
subBind bndr@(NonRec b rhs) = do
    putMsgS $ "Non-recursive binding named " ++ showSDoc (ppr b)
    rhs' <- subExpr rhs
    return (NonRec b rhs')
subBind bndr@(Rec pairs) = do
    mapM (uncurry printRecBind) pairs
    return bndr

printRecBind b _e = do
    putMsgS $ "Recursive binding " ++ showSDoc (ppr b)

subExpr :: CoreExpr -> CoreM CoreExpr

subExpr expr@(Type t) = do
    putMsgS "Type"
    return expr

subExpr expr@(Coercion co) = do
    putMsgS "Coercion"
    return expr

subExpr expr@(Lit lit) = do
    putMsgS $ "Lit " ++ showSDoc (ppr lit)
    return expr

subExpr expr@(Var v) = do
    putMsgS "Var"
    return expr

subExpr (App f a) = do
    let funcName = showSDoc (ppr f)
    putMsgS $ "App " ++ funcName
    f' <- subExpr f
    a' <- subExpr a
    collapse (App f' a')

subExpr (Tick t e) = do
    putMsgS "Tick"
    e' <- subExpr e
    return (Tick t e')

subExpr (Cast e co) = do
    putMsgS "Cast"
    e' <- subExpr e
    return (Cast e' co)

subExpr (Lam b e) = do
    putMsgS "Lam"
    e' <- subExpr e
    return (Lam b e')

subExpr (Let bind e) = do
    putMsgS "Let"
    bind' <- subBind bind
    e' <- subExpr e
    return (Let bind' e')

subExpr expr@(Case scrut bndr ty alts) = do
    putMsgS "Case"
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
mkUnaryCollapseIEEE fnE expr@(App f1 (App f2 (Lit (MachDouble d))))
    | isDHash f2 = do
        let sub = fnE (fromRational d)
        maybe (return expr) (\x -> return (App f2 (mkDoubleLitDouble x)))
              =<< maybeIEEE (fromJust $ funcName f1) sub
mkUnaryCollapseIEEE fnE expr@(App f1 (App f2 (Lit (MachFloat d))))
    | isFHash f2 = do
        let sub = fnE (fromRational d)
        maybe (return expr) (\x -> return (App f2 (mkFloatLitFloat x)))
              =<< maybeIEEE (fromJust $ funcName f1) sub
mkUnaryCollapseIEEE _ expr = return expr

mkUnaryCollapseNum :: (forall a . Num a => (a -> a))
                   -> CoreExpr
                   -> CoreM CoreExpr
mkUnaryCollapseNum fnE expr@(App f1 (App f2 (Lit (MachDouble d))))
    | isDHash f2 = do
        let sub = fnE (fromRational d)
        return (App f2 (mkDoubleLitDouble sub))
mkUnaryCollapseNum fnE expr@(App f1 (App f2 (Lit (MachFloat d))))
    | isFHash f2 = do
        let sub = fnE (fromRational d)
        return (App f2 (mkFloatLitFloat sub))
mkUnaryCollapseNum fnE expr@(App f1 (App f2 (Lit (MachInt d))))
    | isIHash f2 = do
        let sub = fnE (fromIntegral d)
        putMsgS $ "Replacing " ++ (fromJust $ funcName f1)
        return (App f2 (mkIntLitInt sub))
mkUnaryCollapseNum fnE expr@(App f1 (App f2 (Lit (MachWord d))))
    | isWHash f2 = do
        let sub = fnE (fromIntegral d)
        putMsgS $ "Replacing " ++ (fromJust $ funcName f1)
        return (App f2 (mkWordLitWord sub))
mkUnaryCollapseNum _ expr = return expr

mkBinaryCollapse :: (forall a. RealFloat a => (a -> a -> a))
                 -> CoreExpr
                 -> CoreM CoreExpr
mkBinaryCollapse fnE expr@(App (App f1 (App f2 (Lit (MachDouble d1)))) (App f3 (Lit (MachDouble d2))))
    | isDHash f2 && isDHash f3 = do
        let sub = fnE (fromRational d1) (fromRational d2)
        maybe (return expr) (\x -> return (App f2 (mkDoubleLitDouble x)))
              =<< maybeIEEE (fromJust $ funcName f1) sub
mkBinaryCollapse fnE expr@(App (App f1 (App f2 (Lit (MachFloat d1)))) (App f3 (Lit (MachFloat d2))))
    | isFHash f2 && isFHash f3 = do
        let sub = fnE (fromRational d1) (fromRational d2)
        maybe (return expr) (\x -> return (App f2 (mkDoubleLitDouble x)))
              =<< maybeIEEE (fromJust $ funcName f1) sub
mkBinaryCollapse _ expr = return expr

fromRationalCollapse :: CoreExpr -> CoreM CoreExpr
fromRationalCollapse expr@(App f1 (App (App f2 (Lit (LitInteger n _))) (Lit (LitInteger d _))))
    | Just "GHC.Real.fromRational" <- funcName f1
    , Just "GHC.Real.:%" <- funcName f2
      = do
          let sub = fromRational $ (fromInteger n) / (fromInteger d)
          maybe (return expr) (\x -> return (App f2 (mkFloatLitFloat x))) =<< maybeIEEE (fromJust $ funcName f1) sub
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
    , CMSub    "GHC.Real.fromRational" fromRationalCollapse
    ]

subFunc :: Map String CMSub
subFunc = Map.fromList $ zip (map cmFuncName subs) subs
