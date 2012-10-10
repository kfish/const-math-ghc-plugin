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
    a' <- subExpr a
    collapseUnary (App f a')

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

collapseUnary :: CoreExpr -> CoreM CoreExpr
collapseUnary expr@(App f1 (App f2 (Lit (MachDouble d))))
    | isDHash f2
    , Just f <- cmSubst <$> findSub f1
    , Just name <- funcName f1
      = maybe (return expr) substUnary =<< maybeIEEE name (f (fromRational d))
    where
        substUnary x = return (App f2 (mkDoubleLitDouble x))
collapseUnary expr@(App f1 (App f2 (Lit (MachFloat d))))
    | isFHash f2
    , Just f <- cmSubst <$> findSub f1
    , Just name <- funcName f1
      = maybe (return expr) substUnary =<< maybeIEEE name (f (fromRational d))
    where
        substUnary x = return (App f2 (mkFloatLitFloat x))
collapseUnary expr = return expr

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
    , cmSubst    :: forall a . (RealFloat a) => (a -> a)
    }

funcName :: CoreExpr -> Maybe String
funcName = listToMaybe . words . showSDoc . ppr

isFHash :: CoreExpr -> Bool
isFHash = maybe False ((==) "GHC.Types.F#") . funcName

isDHash :: CoreExpr -> Bool
isDHash = maybe False ((==) "GHC.Types.D#") . funcName

findSub :: CoreExpr -> Maybe CMSub
findSub = flip Map.lookup subFunc <=< funcName

subs =
    [ CMSub "GHC.Float.exp"    exp
    , CMSub "GHC.Float.log"    log
    , CMSub "GHC.Float.sqrt"   sqrt
    , CMSub "GHC.Float.sin"    sin
    , CMSub "GHC.Float.cos"    cos
    , CMSub "GHC.Float.tan"    tan
    , CMSub "GHC.Float.asin"   asin
    , CMSub "GHC.Float.acos"   acos
    , CMSub "GHC.Float.atan"   atan
    , CMSub "GHC.Float.sinh"   sinh
    , CMSub "GHC.Float.cosh"   cosh
    , CMSub "GHC.Float.tanh"   tanh
    , CMSub "GHC.Float.asinh"  asinh
    , CMSub "GHC.Float.acosh"  acosh
    , CMSub "GHC.Float.atanh"  atanh
    , CMSub "GHC.Num.negate"   negate
    , CMSub "GHC.Num.abs"      abs
    , CMSub "GHC.Num.signum"   signum
    ]

subFunc :: Map String CMSub
subFunc = Map.fromList $ zip (map cmFuncName subs) subs
