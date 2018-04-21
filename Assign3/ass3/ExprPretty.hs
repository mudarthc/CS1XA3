{-|
Module      : ExprPretty
Description : Contains a type class and instances for
              differentiable expressions
Copyright   : (c) Christina Mudarth @2018
License     : WTFPL
Maintainer  : christina.mudarth@mcmaster.ca
Stability   : experimental
Portability : POSIX
ExprPretty uses @parenns@ to return values with brackets, instance show is used to display what to return in accordance to the expression eveluated -}

module ExprPretty where

import           ExprType
-- | parenns takes a string and returns a string, the aim is to evaluate the epression with the brackets as an output
parenns :: String -> String
parenns ss = "(" ++ ss ++ ")"

-- | this instance of show is used to display the string used to identify each function and what to return in accordance to what is inputed
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parenns (show e1) ++ " !* " ++ parenns (show e2)
  show (Add e1 e2)  = parenns (show e1) ++ " !+ " ++ parenns (show e2)

  show (Sub e1 e2)  = parenns (show e1) ++ " !- " ++ parenns (show e2)
  show (Var ss)     = parenns $ "var \"" ++ ss ++ "\""
  show (Const x)    = parenns $ "val " ++ show x
  show (Exp e1 e2) = parenns $ (show e1)++"!^"++(show e2)
  show (E e1) = parenns $ "e^"++(show e1)
  show (Cos a) = parenns $ "cos" ++ (show a)
  show (Sin a) = parenns $ "sin" ++ (show a)
  show (Acos a) = parenns $ "acos" ++ (show a)
  show (Asin a) = parenns $ "asin" ++ (show a)
  show (Neg a) = parenns $ "neg" ++ (show a)