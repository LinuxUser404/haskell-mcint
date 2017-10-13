{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module FunctionParser (parseIntegrate) where

import Data.List
import Text.ParserCombinators.Parsec

import FunctionTypes

{-
Takes stream of chars and converts it(reads from it) to an object which
contains the function and its limits of integration
-}
parseIntegrate :: GenParser Char st FunctionExpression
parseIntegrate = do
  spaces
  string "Integrate"
  spaces
  char '['
  spaces
  expr <- parseExpression
  spaces
  char ','
  spaces
  vars <- parseVariables
  spaces
  char ']'
  return $ FunctionExpression "Integrate" expr vars
  --example input: "Integrate[x1 * x2 + x2, {x1,0,10}, {x2,0,20}]"  
  --example output: FunctionExpression "Integrate" "x1 * x2 + x2" [("x1", Limits 0.0 10.0),("x2", Limits 0.0 20.0)]

parseExpression :: GenParser Char st String
parseExpression = many (noneOf("[,{}]")) >>= \str -> return str

-- parseVariable, if comma follows perform recursive call, otherwise return list with single element var
parseVariables :: GenParser Char st [(String, Limits)]
parseVariables = spaces >> parseVariable >>= \var -> (((char ',') >> (parseVariables >>= \vars -> return $ var:vars)) <|> (return $ var:[]))


parseVariable :: GenParser Char st (String, Limits)
parseVariable = do
  char '{'                     -- first character in the stream should be '{'
  spaces                       -- then any number(maybe 0) of spaces follow
  str   <- many (noneOf(" ,")) -- a string of characters - the variable name
  spaces
  char ','
  spaces
  lower <- many (noneOf(" ,"))
  spaces
  char ','
  spaces
  upper <- many (noneOf(" }"))
  spaces
  char '}'
  return $ (str, Limits (read lower) (read upper))
