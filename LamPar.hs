{-# LANGUAGE UnicodeSyntax #-}

module LamPar where

import LibPar

-------------
-- Datatypes
-------------

type CN = Int
type VN = String

type Context = CN -> Int -> Expr
type Subst = CN -> VN
type NSubst = VN -> CN

data Expr = Const CN
          | Var CN
          | Lambda Expr
          | App Expr Expr
          deriving Eq

data NExpr = NConst CN
           | NVar VN
           | NLambda VN NExpr
           | NApp NExpr NExpr

-----------
-- Grammar
-----------

lam  = char '\\'
dot  = char '.'
at   = char '@'
lpar = char '('
rpar = char ')'

symbol = anyOf ['a'..'z']

word = many1 symbol

var = word >>= return . NVar

lambda = lam >> var >>= \(NVar x) -> dot >> app >>= \y -> return (NLambda x y)

app = fmap (foldl1 NApp) $ many1SepBy (lambda <|> var <|> between lpar app rpar) at

--------------
-- Simplifier
--------------

findBound :: Expr -> Int -> Expr
findBound (Const c)  _ = Const c
findBound (Var v)    d = Var $ if v == d then (-v) else v
findBound (Lambda m) d = Lambda $ findBound m (d + 1)
findBound (App a b)  d = App (findBound a d) (findBound b d)

updFree :: Expr -> Int -> Int -> Expr
updFree (Const c)  _ _ = Const c
updFree (Var v)    x d = Var $ if v > d then (v + x) else v
updFree (Lambda m) x d = Lambda $ updFree m x (d + 1)
updFree (App a b)  x d = App (updFree a x d) (updFree b x d)

subst :: Expr -> Expr -> Int -> Expr
subst (Const c)  _ _ = Const c
subst m@(Var v)  n d = if v < 0 then updFree n d 0 else m
subst (Lambda m) n d = Lambda $ subst m n (d + 1)
subst (App a b)  n d = App (subst a n d) (subst b n d)

eval :: Bool -> Expr -> Expr
eval f (App a b)  = case (eval f a, eval f b) of
    (Const c,   n) -> Const c `App` n
    (Var v,     n) -> Var v `App` n
    (Lambda m,  n) -> eval f $ subst (updFree (findBound (eval f m) 1) (-1) 1) n 0
    (App a' b', n) -> a' `App` b' `App` n
eval f (Lambda m) = Lambda $ if f then eval f m else m
eval _ m          = m

evalNF = eval True
evalWHNF = eval False

cnil :: Context
cnil x _ = Var x

cappend :: Context -> CN -> Expr -> Context
cappend ctx v m x d = if d - x == v then updFree m (d - v - 1) 0 else ctx x d

ceval :: Expr -> Expr
ceval expr = _ceval expr cnil 0
_ceval (Const  c) _   _ = Const c
_ceval (Var    v) ctx d = ctx v d
_ceval (Lambda m) ctx d = Lambda $ _ceval m ctx (d + 1)
_ceval (App a b) ctx d = case (_ceval a ctx d, _ceval b ctx d) of
    (Const c,   n) -> (Const c) `App` n
    (Var v,     n) -> (Var   v) `App` n
    (Lambda m,  n) -> updFree (_ceval m (cappend ctx d n) (d + 1)) (-1) (d + 1)
    (App a' b', n) -> a' `App` b' `App` n

-- Conversion part (NExpr -> Expr)

nnil :: NSubst
nnil x = error $ "unknown variable '" ++ x ++ "'"

nappend :: NSubst -> VN -> CN -> NSubst
nappend ctx s v x = if x == s then v else ctx x

toExpr :: NExpr -> Expr
toExpr nexpr = _toExpr nexpr nnil 0 where
    _toExpr (NConst c)    _   _ = Const c
    _toExpr (NVar v)      ctx d = Var $ d - (ctx v)
    _toExpr (NLambda v m) ctx d = Lambda $ _toExpr m (nappend ctx v d) (d + 1)
    _toExpr (NApp a b)    ctx d = App (_toExpr a ctx d) (_toExpr b ctx d)

instance Show Expr where
    show (Const c)          = show c
    show (Var v)            = show v
    show (Lambda m)         = "λ " ++ (show m)
    show (App a' b')        = (_showl a') ++ " " ++ (_showr b') where
        _showl a@(Lambda _) = "(" ++ (show a) ++ ")"
        _showl a            = show a
        _showr b@(Lambda _) = "(" ++ (show b) ++ ")"
        _showr b@(App _ _)  = "(" ++ (show b) ++ ")"
        _showr b            = show b

-- Conversion part (Expr -> NExpr)

nil :: Subst
nil x = error $ "unknown variable '" ++ (show x) ++ "'"

genVar :: Int -> String
genVar x = ['x', 'y', 'z', 'v', 't'] !! q : iterate ('\'':) "" !! p where p = x `div` 5
                                                                          q = x `mod` 5
append :: Subst -> CN -> VN -> Subst
append ctx v s x = if x == v then s else ctx x

toNExpr :: Expr -> NExpr
toNExpr expr = _toNExpr expr nil 0 where
    _toNExpr (Const c)  _   _ = NConst c
    _toNExpr (Var v)    ctx d = NVar $ if v <= d then ctx (d - v) else genVar (v - 1)
    _toNExpr (Lambda m) ctx d = NLambda v $ _toNExpr m (append ctx d v) (d + 1) where v = genVar d
    _toNExpr (App a b)  ctx d = NApp (_toNExpr a ctx d) (_toNExpr b ctx d)

instance Show NExpr where
    show (NConst c)             = show c
    show (NVar v)               = v
    show (NLambda a' b')        = "λ " ++ vp ++ " -> " ++ mp where
        (vp, mp)                = _show a' b'
        _show v (NLambda v' m') = (v ++ " " ++ vp', mp') where (vp', mp') = _show v' m'
        _show v m               = (v, show m)
    show (NApp a' b')           = (_showl a') ++ " " ++ (_showr b') where
        _showl a@(NLambda _ _)  = "(" ++ (show a) ++ ")"
        _showl a                = show a
        _showr b@(NLambda _ _)  = "(" ++ (show b) ++ ")"
        _showr b@(NApp _ _)     = "(" ++ (show b) ++ ")"
        _showr b                = show b

--------
-- Main
--------

main = do
    putStrLn "Über simplifier"
    putStr   "Enter your lambda: "
    input <- getLine
    let nexpr = parse app input
    let deBruijn = toExpr nexpr
    let deBruijn' = evalNF deBruijn
    let nexpr' = toNExpr deBruijn'
    putStrLn $ "De-Bruijn representation: " ++ show deBruijn
    putStrLn $ "Simplified de-Bruijn: " ++ show deBruijn'
    putStrLn $ "Simplified lambda: " ++ show nexpr'
