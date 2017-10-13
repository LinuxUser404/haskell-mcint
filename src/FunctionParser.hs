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
Takes String and converts it to and object which
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
  


parseExpression :: GenParser Char st String
parseExpression = many (noneOf("[,{}]")) >>= \str -> return str

parseVariables :: GenParser Char st [(String, Limits)]
parseVariables = spaces >> parseVariable >>= \var -> (((char ',') >> (parseVariables >>= \vars -> return $ var:vars)) <|> (return $ var:[]))
  

parseVariable :: GenParser Char st (String, Limits)
parseVariable = do
  char '{'
  spaces
  str   <- many (noneOf(" ,"))
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
