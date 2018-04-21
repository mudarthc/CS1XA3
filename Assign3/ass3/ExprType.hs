{-|
Module      : ExprType
Description : contains the data expression type of the following calculus helper
Copyright   : (c) CHristina Mudarth @2018
License     : WTFPL
Maintainer  : christina.mudarth@mcmaster.ca
Stability   : experimental
Portability : POSIX
This is an data type of the expressions used in this project, which include all the functions used and needed in the evaluaitons of expressions. 
@getVars@ is used get the variables in each function to evaluate in Map.Map, Lastly @e@ is just used to define what e is as a double -}

module ExprType where

import           Data.List

-- | the data type is used to define the functions used and what it takes 
data Expr a = Add (Expr a) (Expr a) -- ^ Add takes two expression 
            | Mult (Expr a) (Expr a) -- ^ Mult takes two expression 
            | Sub (Expr a) (Expr a) -- ^ Sub takes two expressions
            | Const a -- ^ Const is just some a 
            | Cos (Expr a) -- ^ Cos takes one expression 
            | Sin (Expr a) -- ^ Sin takes one expression 
            | Acos (Expr a) -- ^ Acos takes one expression 
            | Asin (Expr a)-- ^ Asin takes one expression 
            | Ln (Expr a) -- ^ Ln takes one expression 
            | E (Expr a)-- ^ E takes one expression 
            | Var String -- ^ Var takes some string
            |Exp (Expr a) (Expr a)-- ^ Exp takes two expressions
            | Neg (Expr a) -- ^ Neg takes one expression
  deriving Eq
-- | @getVars@ takes and expression and returns a list of string, this is needed for evaluation when using Map.Map in ExprDiff
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 ++ getVars e2
getVars (Mult e1 e2) = getVars e1 ++ getVars e2
getVars (Sub e1 e2) = getVars e1 ++ getVars e2
getVars (Exp e1 e2) = getVars e1 ++ getVars e2
getVars (Neg e1) = getVars e1 
getVars (Cos a) =getVars a
getVars (Sin a) = getVars a
getVars (Acos a) =getVars a
getVars (Asin a) = getVars a
getVars (E a) = getVars a
getVars (Const _)    = []
getVars (Var ident)  = [ident]
-- | @e@ is defined as a double which is exp 1
e :: Double 
e = exp 1
