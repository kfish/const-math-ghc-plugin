
module ConstMath.Pass (
      constMathProgram
) where

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
    case (f'm, kf2) of
        (Just f, Just DHash) -> do
            let sub = f (fromRational d)
            maybe (return expr) (\x -> return (App f2 (mkDoubleLitDouble x))) =<< maybeIEEE (show (fromJust kf1)) sub
        _ -> return expr
    where
        kf1 = toKnownFunc f1
        kf2 = toKnownFunc f2
        f'm = flip Map.lookup subFunc =<< kf1
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

type CMEnv = Map KnownFunc CoreExpr
data KnownFunc = DHash | Sqrt | Exp | Log | Sin | Cos | Negate
    deriving (Ord, Eq, Show)

toKnownFunc :: CoreExpr -> Maybe KnownFunc
toKnownFunc expr
    | isPrefixOf "GHC.Types.D#"   funcName = Just DHash
    | isPrefixOf "GHC.Float.sqrt" funcName = Just Sqrt
    | isPrefixOf "GHC.Float.exp"  funcName = Just Exp
    | isPrefixOf "GHC.Float.log"  funcName = Just Log
    | isPrefixOf "GHC.Float.sin"  funcName = Just Sin
    | isPrefixOf "GHC.Float.cos"  funcName = Just Cos
    | isPrefixOf "GHC.Num.negate" funcName = Just Negate
    | otherwise                            = Nothing
    where
        funcName = showSDoc (ppr expr)

subFunc :: Map KnownFunc (Double -> Double)
subFunc = Map.fromList
    [ (Sqrt, sqrt)
    , (Exp, exp)
    , (Log, log)
    , (Sin, sin)
    , (Cos, cos)
    , (Negate, negate)
    ]
