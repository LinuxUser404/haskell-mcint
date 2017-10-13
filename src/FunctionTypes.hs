{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module FunctionTypes (FunctionExpression(..),Limits(..)) where 

import Foreign.C.Types( CFloat )

{-
data IntegrateFunction = IntegrateFunction {
  limits :: Num a -> (a, a)
}
-}

data FunctionExpression = FunctionExpression {
  name       :: String,
  expression :: String,
  variables  :: [(String, Limits)]
} deriving Eq

data Limits = Limits {
  lower :: CFloat,
  upper :: CFloat
} deriving Eq
