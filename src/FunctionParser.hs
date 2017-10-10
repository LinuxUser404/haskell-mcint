{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module FunctionParser (parseFunction) where


import Text.ParserCombinators.Parsec

{-
Takes String and converts it to and object which
contains the function and its limits of integration
-}
parseFunction
