{-# LANGUAGE GADTs
           , StandaloneDeriving
           #-}

module Debug.Reflect where

data Expr a where
    NumExpr   :: (Eq a, Num a) => a -> Expr a
    VarExpr   :: String -> Expr a
    AppExpr   :: Expr (a -> b) -> Expr a -> Expr b
    PlusExpr  :: (Eq a, Num a) => Expr (a -> a -> a)
    MinExpr   :: (Eq a, Num a) => Expr (a -> a -> a)
    MultExpr  :: (Eq a, Num a) => Expr (a -> a -> a)
    DivExpr   :: (Eq a, Num a) => Expr (a -> a -> a)

var :: String -> Expr a
var = VarExpr

reduce :: (Eq a, Num a) => Expr a -> Expr a
reduce (AppExpr (AppExpr PlusExpr (NumExpr 0)) e) = e
reduce (AppExpr (AppExpr PlusExpr e) (NumExpr 0)) = e
--reduce (AppExpr (AppExpr MinExpr (NumExpr 0) (NumExpr x))) = NumExpr (negate x)
--reduce (AppExpr (AppExpr MinExpr e (NumExpr 0)))
--reduce (InfixExpr (VarExpr "+") e (NumExpr 0))           = e
--reduce (InfixExpr (VarExpr "*") e (NumExpr 0))           = NumExpr 0
--reduce (InfixExpr (VarExpr "*") (NumExpr 0) e)           = NumExpr 0
--reduce (InfixExpr (VarExpr "*") e (NumExpr 1))           = e
--reduce (InfixExpr (VarExpr "*") (NumExpr 0) e)           = e
--reduce (InfixExpr (VarExpr "-") (NumExpr 0) (NumExpr x)) = NumExpr (negate x)
--reduce (InfixExpr (VarExpr "-") e (NumExpr 0))           = e
reduce e                                                 = e
