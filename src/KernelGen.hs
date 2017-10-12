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
               ++ "(" ++ (intercalate ", " (outarg:args) ) ++ ")"
               ++ "{"
               ++ "int id = get_global_id(0);"
               ++ "out[id] = " ++ fexpr ++ ";"
               ++ "}"
  where
    args   = map ((++) "__global float *") (variables expr) :: [String]
    outarg = "__global float *out"
    fexpr = substitute (expression expr) (variables expr) "[id]"
    

-- write "[id]" instead of "#"
substitute :: String -> [String] -> String -> String
substitute [] _ _ = []
substitute ('#':st) vars ids = ids ++ (substitute st vars ids)
substitute expr vars ids = (takeWhile (/='#') expr) ++ (substitute (dropWhile (/='#') expr) vars ids)
