{-|
Module      : ExprParser
Description : Contains a parser for double or float to increase accuracey 
              differentiable expressions
Copyright   : (c) Christina Mudarth @2018
License     : WTFPL
Maintainer  : christina.mudarth@mcmaster.ca
Stability   : experimental
Portability : POSIX
parses using @parseExprD@ and @parseExprF@ are parsers used to parse a string into float or double. This parser has the ability to search for key words ex Const Var .. and convert it into an expression which can be evaluated -}
module ExprParser where


import           ExprType

import           Text.Parsec
import           Text.Parsec.String
import qualified Data.Map as Map

-- *** Top Parsers for Double and Float
parseExprD :: String -- ^ The string to be parsed 
            -> Expr Double -- ^ The resulting parsed Double Expr
parseExprD ss = case parse dFactor "" ("("++ss++")") of
                    Left err -> error (show err)
                    Right expr -> expr 

parseExprF:: String  -- ^ The string to be parsed 
            -> Expr Float -- ^ The resulting parsed Float Expr
parseExprF ss = case parse fFactor "" ("("++ss++")") of
                    Left err -> error (show err)
                    Right expr -> expr



-- *** Lower Parsers for float and double


dFactor :: Parser (Expr Double)  -- ^ Parse expression as a Double 
dFactor = try (parens $ setexpr dFactor) <|> try dConst <|> var

dConst = do {
            d <- double;
            return (Const d)}

fFactor :: Parser (Expr Float)  -- ^ Parse expression as a Float 
fFactor = try (parens $ setexpr fFactor) <|> try fConst <|> var

-- parses for a float and creates into a Constant float 
fConst = do {f <- float;
            return (Const f)}

var :: Parser (Expr a) -- ^ parses expression for Var ss 
var = do {  spaces;
            a <-iden;
            spaces;
            return (Var a)}

iden :: Parser String -- ^ parses string for alphaNum
iden = many1 alphaNum







-- parses for all trig identities and all functions 
setexpr :: Parser (Expr a) -- ^ parses expression as an input
        -> Parser (Expr a) -- outputs a parser expression 
setexpr a = (alltrig a)  `chainl1` allfunc


-- chack all functions if in string 
allfunc ::  Parser (Expr a -> Expr a -> Expr a) -- ^ parses for functions such as @!-@ @!+@ @!^@ @!*@ 
allfunc = (do {symbol "!*"; return (Mult)}) <|> (do {symbol "!+"; return (Add)}) <|> (do {symbol "!-"; return (Sub)}) <|> (do {symbol "!^"; return (Exp)})


-- | evaluates for the number involved in trig expression, input is a string of the name of the trig ident and value to evaluate, expr a to expr a, parses expr a, return parser expr a 
trigOp :: String -> --  input is a string of the name of the trig ident and value to evaluate
        (Expr a -> Expr a) -> --  expr a to expr a
        Parser (Expr a) -> --  parses expr a
        Parser (Expr a) --  return parser expr a 
trigOp name alltrig num = do {symbol name;
                            expr <- num;
                         return (alltrig expr)}


alltrig :: Parser (Expr a) -- ^ input is a parser of a cetrian expression
    -> Parser (Expr a) -- ^ output a parser expression, aim is to match any of the trig expressions with the input 
alltrig a = mycos a <|> mysin a <|> myasin a <|> myln a <|> mye a <|> a 


mycos :: Parser (Expr a) -- ^ input is a parser expression 
    -> Parser (Expr a) -- ^ output is also a parser expression, aim is to chack if "cos" is in the string input originally 
mycos a = try (trigOp "cos" Cos a)

mysin :: Parser (Expr a)-- ^ input is a parser expression 
    -> Parser (Expr a)-- ^ output is also a parser expression, aim is to chack if "sin" is in the string input originally 
mysin a = try (trigOp "sin" Sin a) 

myacos :: Parser (Expr a) -- ^ input is a parser expression 
    -> Parser (Expr a)-- ^ output is also a parser expression, aim is to chack if "acos" is in the string input originally 
myacos a = try (trigOp "acos" Asin a)


myasin:: Parser (Expr a)-- ^ input is a parser expression 
    -> Parser (Expr a)-- ^ output is also a parser expression, aim is to chack if "asin" is in the string input originally 
myasin a = try (trigOp "asin" Acos a)

myln :: Parser (Expr a) -- ^ input is a parser expression 
    -> Parser (Expr a)-- ^ output is also a parser expression, aim is to chack if "ln" is in the string input originally 
myln a = try (trigOp "ln" Ln a) 

mye:: Parser (Expr a) -- ^ input is a parser expression 
    -> Parser (Expr a)-- ^ output is also a parser expression, aim is to chack if "tinaE" is in the string input originally 
mye a = try (trigOp "tinaE" E a)


parens :: Parser a -- ^input is a parser a
    -> Parser a -- Output is a parser a, aim is to parse out the "( )" in the original string 
parens p = do { symbol "(";
                cs <- p;
                symbol ")";
                return cs }


symbol :: String -- ^ input is a string 
    -> Parser String-- ^ output is a parser string, aim is to parse out the spaces in the given string for an input 
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

-- *** Utility Parsers



dec :: Parser String -- ^ this ia a parcer string that is used to parce decimals using the "." (found similar in Curtis' code)
dec = do {
            char '.';
            a <- digits;
            return ("."++a)}

fore :: Parser String -- ^ parses string for e by trying to see if its a @digits@ or @negDigit@, and checking if theres a char e  
fore = do {
            char 'e';
            e <- (try negDigits<|>digits);
            return ("e"++e);
            }

nil :: Parser String -- ^ checks if theres nothing in the string
nil = do {return ""}


decimal :: Parser String -- ^ parses string for decimal using @dec@ , @nill@ and @fore@ 
decimal = do {r <- digits;
                a <- (try dec<|>nil);
                e <- (try fore<|>nil);
            return (r++a++e)}

negDecimal :: Parser String
negDecimal = do {neg <- symbol "-";
                dec <- decimal;
            return (neg++dec)}


float :: Parser Float -- ^ parses string for float  
float = fmap read $ try negDecimal <|> decimal

double :: Parser Double -- ^ parses string for double 
double = fmap read $ try negDecimal <|> decimal


digits :: Parser String -- ^ parses string for digits 
digits = many1 digit

negDigits :: Parser String -- ^ parses strign for negative digits using "-" symbol 
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }







