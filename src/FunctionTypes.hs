{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module FunctionTypes (FunctionExpression(..)) where 

{-
data IntegrateFunction = IntegrateFunction {
  limits :: Num a -> (a, a)
}
-}

data FunctionExpression = FunctionExpression {
  name       :: String,
  expression :: String,
  variables  :: [String]
} deriving Eq
