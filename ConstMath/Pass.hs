
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

subExpr _expr@(App f a) = do
    let funcName = showSDoc (ppr f)
    putMsgS $ "App " ++ funcName
    a' <- subExpr a
    collapseUnary (App f a')

subExpr expr@(Tick t e) = do
    putMsgS "Tick"
    return expr

subExpr expr@(Cast e co) = do
    putMsgS "Cast"
    return expr

subExpr expr@(Lam b e) = do
    putMsgS "Lam"
    return expr

subExpr _expr@(Let bind e) = do
    putMsgS "Let"
    bind' <- subBind bind
    e' <- subExpr e
    return (Let bind' e')

subExpr expr@(Case scrut bndr ty alts) = do
    putMsgS "Case"
    return expr

----------------------------------------------------------------------

collapseUnary :: CoreExpr -> CoreM CoreExpr
collapseUnary expr@(App f1 (App f2 (Lit (MachDouble d)))) =
    case (f'm, dh) of
        (Just f, True) -> do
            let sub = f (fromRational d)
            maybe (return expr) (\x -> return (App f2 (mkDoubleLitDouble x))) =<< maybeIEEE (fromJust $ funcName f1) sub
        _ -> return expr
    where
        f'm = cmSubst <$> findSub f1
        dh  = isDHash f2
collapseUnary expr = return expr

maybeIEEE :: String -> Double -> CoreM (Maybe Double)
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
    , cmSubst    :: (Double -> Double)
    }

funcName :: CoreExpr -> Maybe String
funcName = listToMaybe . words . showSDoc . ppr

isDHash :: CoreExpr -> Bool
isDHash = maybe False ((==) "GHC.Types.D#") . funcName

findSub :: CoreExpr -> Maybe CMSub
findSub = flip Map.lookup subFunc <=< funcName

subs =
    [ CMSub "GHC.Float.sqrt"   sqrt
    , CMSub "GHC.Float.exp"    exp
    , CMSub "GHC.Float.log"    log
    , CMSub "GHC.Float.sin"    sin
    , CMSub "GHC.Float.cos"    cos
    , CMSub "GHC.Num.negate"   negate
    ]

subFunc :: Map String CMSub
subFunc = Map.fromList $ zip (map cmFuncName subs) subs
