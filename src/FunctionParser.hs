{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module FunctionParser (parseIntegrate) where

import Data.List
import Text.ParserCombinators.Parsec

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative((<*))

import FunctionTypes

{-
Takes stream of chars and converts it(reads from it) to an object which
contains the function and its limits of integration
-}

parseIntegrate :: Parser FunctionExpression
parseIntegrate = spaces >> (m_reserved "Integrate") >> do
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

parseExpression :: Parser String
parseExpression = many (noneOf("[,{}]")) >>= \str -> return str

-- parseVariable, if comma follows perform recursive call, otherwise return list with single element var
parseVariables :: Parser [(String, Limits)]
parseVariables = spaces >> (braces lexer parseVariable) >>= \var -> (((char ',') >> (parseVariables >>= \vars -> return $ var:vars)) <|> (return $ var:[]))

--parseVariableWithB = braces parseVariable

parseVariable :: Parser (String, Limits)
parseVariable = do 
  str <- identifier lexer
  spaces
  char ','
  spaces
  lower <- float lexer
  spaces
  char ','
  spaces
  upper <- float lexer
  return $ (str, Limits lower upper)

opNames = ["+","-","*","/"]

--myLangDef = haskellDef
myLangDef = emptyDef{ commentStart = "(*"
              , commentEnd = "*)"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:+-*/"
              , opLetter = oneOf "~&=:+-*/"
              , reservedOpNames = ["+", "-", "*", "/"]
              , reservedNames = ["True", "False",
                                 "Integrate",
                                 "Cos", "Sin", "Exp", "Log"]
              , caseSensitive = True
              }

lexer@(TokenParser{ parens = m_parens, identifier = m_identifier, reservedOp = m_reservedOp, reserved = m_reserved, semiSep1 = m_semiSep1, whiteSpace = m_whiteSpace }) = makeTokenParser myLangDef


data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr deriving Show
data Unop = Cos | Sin | Exp | Log deriving Show
data Duop = Plus | Minus | Times | Divide deriving Show
--data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt | Seq [Stmt] deriving Show

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Infix (m_reservedOp "+" >> return (Duo Plus )) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Minus)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Times)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Divide)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "True" >> return (Con True))
       <|> (m_reserved "False" >> return (Con False))
{-
mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (While b p)
                     }
-}
{-
play :: String -> IO ()
play inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }
-}
