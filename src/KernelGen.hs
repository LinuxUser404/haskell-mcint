{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module KernelGen where

import Data.List(intercalate)

import FunctionTypes

genKernel :: FunctionExpression -> String
genKernel expr = "__kernel " ++ "void " ++ (name expr)
               ++ "(" ++ (intercalate ", " (args `mappend` (outarg:[])) ) ++ ")"
               ++ "{"
               ++ "int id = get_global_id(0);"
               ++ "out[id] = " ++ (expression expr) ++ "[id]" ++ ";"
               ++ "}"
  where
    args   = map ((++) "__global float *") (variables expr) :: [String]
    outarg = "__global float *out"
    
