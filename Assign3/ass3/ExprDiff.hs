{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : ExprDiff
Description : Contains a type class and instances for
              differentiable expressions, integrals, and surface area. Includes functions for simple computations.
Copyright   : (c) Christina Mudarth @2018
License     : WTFPL
Maintainer  : christina.mudarth@mcmaster.ca
Stability   : experimental
Portability : POSIX
This module is geared towards calculus. The differential expression which include @partDiff@ and @deriv@, which contain type classes and instances.
There also also anti derivative present @intr@, to aide in surface area computation there is @areay@ and @areax@ which has a type class and instances to perform calculus calculations.
There is basic functions for computiations in type class and instances @!+@, @!*@, @!-@, @!^@, @tinaCos@, @tinaSin@, @tinaAcos@, @tinaAsin@, @tinaLn@, @neg@. These are all simplified and evaluated with the @eval@ and @simplify@ classes.
My special features that i included are my @!-@; subtraction, @!^@; a^ b, @tinaCos@; cosine, @tinaSin@; sine, @tinaAcos@; acos, @tinaAsin@; acos, @tinaLn@; log , @neg@; negates. 
Also included usefull tools such as @intr@; antiderivative, @deriv@; derivatives, @areay@; surface area of revolution on y,  and @areax@ ; surface area of revolution on x.
 -}

module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map


data EvalResult a = 
  EvalError String -- ^ evaluates the input and returns the result
  | Result a -- ^ data type evaluates result as either and returns the error 
  deriving Show

-- | instance functor to evaluate the function which checks for error or answer; returns the result if succesfully evaluated as a fuction, or returns the error if unable to be evaluated 
instance Functor EvalResult where 
  -- | returns the result if succesfully evaluated as a fuction
  fmap f (Result x) = Result (f x) 
  -- | returns the error if unable to be evaluated 
  fmap f (EvalError err) = EvalError err 
  
-- |instance applicative to evaluate the input 
instance Applicative EvalResult where

  pure x = Result x
  -- ^if pure input the result is the input returned again
  
  (Result f) <*> x = fmap f x
  -- ^return fmap of the function if input is able to be evaluated
  (EvalError err) <*> _ = EvalError err
  -- ^if the inpt is unable to be evaluated then return thr error message

-- |type class of certian expressions in calculus
class DiffExpr a where 
  eval :: Map.Map String a -- ^evaluates input as Map.Map string a 
    -> Expr a -- ^ evaluates the expression
     -> a -- ^return some result of the differential equation
  
  simplify :: Map.Map String a --  ^ simplifies an expression as much as it can, as a Map.Map string a 
    -> Expr a -- ^ input expression
    -> Expr a -- ^ returns a simplified expression 
  
  partDiff ::String -- ^ input is a string of var to be evaluated 
    -> Expr a -- ^ input an expression to be deifferentiated, for example @input@ simplify (Map.singleton "x" 1) (Sub (Add (Const 35) (Const 7)) (Const 5))
    -> Expr a -- ^ and returns an expression which evaluates the partial differentiation of an equation given some variable 
  
  deriv :: Expr a -- ^ deriv inputs an expresion ex @input@ deriv (Const 2)
    -> Expr a -- ^ simply returns the an expression which represents the derivative of the expression, 
  intr :: Expr a -- ^ intr takes the integral of an expression given some expression 
    -> Expr a -- ^ returns the integral as an expression 
  areay :: Expr a -- ^ areay takes some expression that respresents what y is 
    -> Expr a -- ^ outputs an expression which is the surface area of revolution of an equation about the y axis
  areax :: Expr a -- ^ areay takes some expression that respresents what y is 
    -> Expr a -- ^ outputs an expression which is the surface area of revolution of an equation about the x axis
  -- | takes first expression takes second expression returns a added expression with 
  (!+) :: Expr a -> Expr a -> Expr a 
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  --  given an input of ex Add (Const 1) (Const 5) two inputs it returns a expression of the added inputs 
  -- | takes first expression takes second expression returns a subtracted expression with !-
  (!-) :: Expr a -> Expr a -> Expr a 
  e1 !- e2 = simplify (Map.fromList []) $ Sub e1 e2 
  -- uses Sub to indicate the subtraction of two expressions ex Sub (Const 1) (Const 3)
  -- | takes first expression takes second expression returns a multiplied expression with !*
  (!*) :: Expr a -> Expr a -> Expr a 
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  --  uses Mult to indicate the multiplication of two expressions 
  -- | takes first expression takes second expression returns a multiplied expression with !* ex a ^ b
  (!^) :: Expr a -> Expr a -> Expr a 

  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2
  -- uses Exp to indicate Exp (Const 1) (Const 3) as 1 !^ 3 
  -- | Cos x used to evaluate cosine from Map.fromList and simplifies it 
  tinaCos :: Expr a -- ^ cosine function expression
    -> Expr a -- ^ returns a cosine b evaluated expression 
  tinaCos x = simplify (Map.fromList []) $ Cos x
  -- | Acos x used to evaluate acos from Map.fromList and simplifies it 
  tinaAcos :: Expr a -- ^ acos function expression
    -> Expr a -- ^ returns a acos b evaluated expression 
  tinaAcos x = simplify (Map.fromList []) $ Acos x
  -- | Asin x used to evaluate asin from Map.fromList and simplifies it 
  tinaAsin :: Expr a -- ^ asin function expression input
    -> Expr a -- ^ returns a asin b evaluated expression 
  tinaAsin x = simplify (Map.fromList []) $ Asin x
  -- | Sin x used to evaluate sine from Map.fromList and simplifies it  
  tinaSin :: Expr a -- ^ sin function expression input
    -> Expr a -- ^ returns a sin b evaluated expression 
  tinaSin x = simplify (Map.fromList []) $ Sin x
  -- | Ln x used to evaluate ln from Map.fromList and simplifies it 
  -- *** Other 
  tinaLn :: Expr a -- ^ log function expression input
    -> Expr a -- ^ returns a log b evaluated expression 
  tinaLn x = simplify (Map.fromList []) $ Ln x
  -- | E x used to evaluate e^b from Map.fromList and simplifies it 
  tinaE :: Expr a -- ^ exp function expression input
    -> Expr a -- ^ returns a exp b evaluated expression 
  tinaE x = simplify (Map.fromList []) $ E x
  -- | Const x used to evaluate some constant 
  val :: a -- ^ represents value input
    -> Expr a -- ^ returns a expression, ex given a number 
  val x = Const x
  -- | Var x used to evaluate soem variable 
  var :: String -- ^ string evaluated at some value ex "x"
    -> Expr a -- ^ returns it as an expression 
  var x = Var x
  -- | Neg x used to evaluate negative expression from Map.fromList 
  neg :: Expr a -- ^input is some expression 
    -> Expr a -- ^ returns the negated expression 
  neg x = Const (eval (Map.fromList []) (Neg x))
  


{-| instance of (Floating a, Eq a) in Diff Expr, shows instance of expressions and functions 
eval : evaluates the function with each expression for Add Neg ... this shows the return value
partDiff : uses partial differtiation function to differention in respect to a certian vrs
simplify : simplifies the expression to the its simpliest form which could be a value, a variable or acombination of both
deriv : takes the derivative of a function through pattern matching basic rules of derivatives
intr : also uses pattern matching to take the intergral of some function
areay : finds the surface area of a function about the y axis
areax: finds the surface area of some function revolved about the x axis -}
instance (Floating a, Eq a) => DiffExpr a where
  
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  -- | evaluates Add; evaluates first input and adds it to evaluated second input 
  eval vrs (Neg e) = (-1) * (eval vrs e)
  -- | evaluates Neg; negates the input by multiplying the input by -1 
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  -- | multiplies the evaluated variables e1 and e2 
  eval vrs (Sub e1 e2) = eval vrs e1 - eval vrs e2
  -- | subtracts the evaluated variables e1 and e2 
  eval vrs (E e) = exp (eval vrs e)
  -- | evaluates the evaluated e as e ^e
  eval vrs (Cos e) = cos (eval vrs e)
  -- | evaluates the evaluated e as cos e
  eval vrs (Exp e1 e2)  = (eval vrs e1) ** (eval vrs e2)
  -- | evaluates the evaluated e and e1  as e ** e1
  eval vrs (Sin e) = sin (eval vrs e)
  -- |evaluates the evaluated e as sine (e) 
  eval vrs (Acos e) = acos (eval vrs e)
  -- | evaluates the evaluated e as acos (e)
  eval vrs (Asin e) = asin (eval vrs e)
  -- | evaluates the evaluated e as asin (e)
  eval vrs (Ln e) = log (eval vrs e)
  -- | evaluates the evaluated e as log (e)
  eval vrs (Const x) = x
  -- | evaluates Const x and just x for example Const 5 == 5.
  eval vrs (Var x) = case Map.lookup x vrs of
  --  for variable evaluates if it is a known variable or not through Map.lookup
                      Just v  -> v
                      Nothing -> error "failed lookup in eval"
                         
  partDiff vrs (Const e) = Const 0
  -- | if given any constant returns the constant 0 
  partDiff vrs (Var x) = if x == vrs then (Const 1) else (Const 0)
  --if vrs is equal to x then returns constant 1 else constant 0
  partDiff vrs (Add e1 e2) = Add (partDiff vrs e1) (partDiff vrs e2) 
  -- | for e1 and e2 added add the partial differentiation of e1 and e2 and add
  partDiff vrs (Sub e1 e2)= Sub (partDiff vrs e1) (partDiff vrs e2) 
  -- | for e1 and e2 subtracted subtract the partial differentiation of e1 and e2 and subtract
  partDiff vrs (Mult e1 e2) = Add (Mult e1 (partDiff vrs e2)) (Mult e2 (partDiff vrs e1))
  -- | output = add the e1 multiplied by the partial derivative of e2 and e2 multiplied by the partial derivative of e1 for any e1 and e2 multiplied 
  partDiff vrs (Cos e) = Mult (Neg (Sin e)) (partDiff vrs e)
  -- | returns the negative sin e multiplied by the partial derivative of e for instance of any cos e
  partDiff vrs (Sin e) = Mult (Cos e) (partDiff vrs e)
  -- | output = multiplies cos e by the partial derivative of e for instance of any sine e
  partDiff vrs (E e) = Mult (E e) (partDiff vrs e)
  -- | returns the multiple of E e multiplied by the partial derivative of e for the instance if partDiff of any E e
  partDiff vrs (Ln e) = Mult (Exp e (Const (-1))) (partDiff vrs e) 
  -- | for any ln e returns e ^-1 mulptiplied by the partial diff of e 
  
  partDiff vrs (Exp (Const e) (Var e1)) = Mult (Ln (Const e)) ((Exp (Const e) (Var e1))) 
  -- | for any Const a ^ Var b returns the ln of the constant time the constant to the power of the variable
  partDiff vrs (Exp e e1) = Mult (Mult e1 (Exp e (Add e1 (Const (-1))))) (partDiff vrs e) 
  -- | for any e to the power of e1 returns the partial derivative times e to the power of constant plus 1


  simplify vrs (Const a) = Const a

  simplify vrs (Var a) = Var a

  simplify vrs (Neg a) = Const (eval vrs (Neg a))
  simplify vrs (Sin a) = Const (eval vrs (Sin a))
  simplify vrs (Cos a) = Const (eval vrs (Cos a))
  simplify vrs (Asin a) = Const (eval vrs (Asin a))
  simplify vrs (Acos a) = Const (eval vrs (Acos a))
  simplify vrs (Ln a) = Const (eval vrs (Ln a))

  --simplify vrs (E a) = Const (eval vrs (E a))
  simplify vrs (Exp a b) = Const (eval vrs (Exp (simplify vrs a) (simplify vrs b))) 


  simplify vrs (Add (Const a) (Const b)) = Const (a + b)
  simplify vrs (Add a b) = simplify vrs (Add (simplify vrs a) (simplify vrs b))
  --simplify vrs (Add (Var a) (Var b))= (Mult (Const 2) (Var a)) 

  --simplify vrs (Sub b a) = Sub ((simplify vrs b) (simplify vrs a)) 
  simplify vrs (Sub a (Const 0)) = simplify vrs a 
  simplify vrs (Sub (Const 0) b) = simplify vrs b
  simplify vrs (Sub (Const a) (Const b)) = Const (a - b)
  simplify vrs (Sub (a) (b)) = simplify vrs (Sub (simplify vrs a) (simplify vrs b))

  --simplify vrs (Mult (Const b) a) = if b == 0 then (Const 0) else eval vrs (Mult (Const b) (simplify vrs a)) 
  simplify vrs (Mult (Const a) (Const b))= Const (a*b)
  simplify vrs (Mult (Const 0) b)= simplify vrs b 
  simplify vrs (Mult a (Const 0)) = simplify vrs a 
  simplify vrs (Mult (a) (b)) = (Mult (simplify vrs (a)) (simplify vrs (b)))
 

  simplify vrs (E a) = Const (eval vrs (E a))

  deriv (Const a) = Const 0 
  deriv (Var a) = Const 1 
  deriv (Add e1 e2)= Add (deriv e1) (deriv e2) 
  deriv (Sub e1 e2) = Sub (deriv e1) (deriv e2) 
  deriv (Mult e1 e2) = Add (Mult e1 (deriv e2)) (Mult e2 (deriv e1))
  deriv (Cos e) = Mult (Neg (Sin e)) (deriv e)
  deriv (Sin e) = Mult (Cos e) (deriv e)
  deriv (E e) = Mult (E e) (deriv e)
  deriv (Ln e) = Mult (Exp e (Const (-1))) (deriv e) 
  deriv (Exp (Const e) (Var e1)) = Mult (Ln (Const e)) ((Exp (Const e) (Var e1)))
  deriv (Exp e e1) = Mult (Mult e1 (Exp e (Add e1 (Const (-1))))) (deriv e) 

  
  intr (Const e) = Mult (Const e) (Var "x") 
  intr (Add e1 e2)= Add (intr e1) (intr e2) 
  intr (Sub e1 e2) = Sub (intr e1) (intr e2) 
  intr (Mult (Const e1) (Const e2)) = intr (Const (e1 +e2))
  intr (Mult (Var "x") (Const a)) = Mult (Exp (Const (a +1)) (Const (-1))) (Exp (Var "x") (Const (a+1)))
  intr (Var "x") = Mult (Exp (Const 2) (Const (-1))) (Exp (Var "x") (Const 2))
  intr (Sin e) = Mult (Neg (Cos e)) (intr e)
  intr (Cos e) = Mult (Sin e) (intr e)
  intr (Ln (Const e)) = Mult (Var "x") (Ln (Const e)) 
  intr (Exp (Const e) (Const e1)) = Mult ((Exp (Const e) (Const e1))) (Var"x")
  intr (Exp (Var "x") (Const e) ) = Mult (Exp (Var "x") (Const (e +1))) (Exp (Const (e +1)) (Const (-1)))
  intr (Exp (Const e) (Var e1)) = Mult (Mult (Const e) (Var e1)) ((Exp) (Mult (Ln (Const e)) (intr (Var e1))) (Const (-1)))
  intr (E (Const e)) = Mult (E (Const e)) (Var "x")
  intr (E (Var e)) = Mult (E (Var e)) (intr (Var e))
  areay (y) = Mult (Mult (Mult (Const 2) (Const pi)) (Var "x")) (Exp (Add (Const 1) (Exp (deriv y) (Const 2))) (Const 0.5)) 
  areax (y) = Mult (Mult (Mult (Const 2) (Const pi)) (y)) (Exp (Add (Const 1) (Exp (deriv y) (Const 2))) (Const 0.5)) 



