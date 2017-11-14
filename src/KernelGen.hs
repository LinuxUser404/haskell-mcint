{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module KernelGen where

import Data.List(intercalate)

import FunctionTypes

genKernel :: FunctionExpression -> String
genKernel expr = "#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n"
               ++ "__kernel " ++ "void " ++ (name expr)
               ++ "(" ++ (intercalate ", " (args ++ outarg:[]) ) ++ ")"
               ++ "{"
               ++ "int id = get_global_id(0);"
               ++ "out[id] = " ++ fexpr ++ ";"
               ++ "}"
  where
    args   = map (((++) "__global double *") . fst) (variables $ expr) :: [String]
    outarg = "__global double *out"
    fexpr = substitute (expression expr) ( variables expr) "[id]"
    

-- write "[id]" instead of "#"
substitute :: String -> [(String, Limits)] -> String -> String
substitute [] _ _ = []
substitute ('#':st) vars ids = ids ++ (substitute st vars ids)
substitute expr vars ids = (takeWhile (/='#') expr) ++ (substitute (dropWhile (/='#') expr) vars ids)
