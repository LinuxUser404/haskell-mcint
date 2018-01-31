{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module KernelGen where

import Data.List(intercalate)

import FunctionTypes

genKernel :: FunctionExpression -> String
genKernel expr = {-"#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n"
	++ -}      "__kernel " ++ "void " ++ (name expr)
               ++ "(" ++ farg ++ ")"
               ++ "{"
               ++ limits
               ++ "int id = get_global_id(0);"
               ++ "double output = 0;"
               ++ (foldedLoops dim subDiv body)
               ++ "out[id] = output / (" ++ intercalate ("*") (replicate dim (show subDiv)) ++");"
               ++ "}"
  where
    subDiv :: Int
    subDiv = 2^10
    args  = concatMap var [0..dim-1]
    limit i = "const double lower" ++ (show i) ++ " = " ++ (show $ lower . snd $ (variables expr)!!i) ++ ";"
           ++ "const double upper" ++ (show i) ++ " = " ++ (show $ upper . snd $ (variables expr)!!i) ++ ";"
    limits = concatMap limit [0..dim-1]
    farg  = "__global const double *x, __global double *out"
    fexpr = expression expr
    dim   = length $ variables expr
    var :: Int -> String
    var i = "const double " ++ (fst $ (variables expr)!!i) ++
         concat [" = ",li i," + (",ui i," - ",li i,") * (",ii i," + ",xi i,") / ",show subDiv,";"]
    loop :: String -> Int -> String -> String
    -- loop "i" 1000 "" = "for(int i = 0; i < 1000; ++i){}"
    loop i n body' = "for(int " ++ i ++ " = 0; " ++ i ++ " < " ++ (show n) ++ "; ++" ++ i ++ "){" ++ body' ++"}"
    foldedLoops 0 _ body' = body'
    foldedLoops depth n body' = loop ("i" ++ (show (depth - 1))) n (foldedLoops (depth - 1) n body')
    body = args ++ "output += " ++ fexpr ++ ";"
    loopVarsList = map (("i" ++) . show) [0..dim-1]
    li i = "lower" ++ (show i)
    ui i = "upper" ++ (show i)
    ii i = "i" ++ (show i)
    xi i = "x[id + " ++ (show i) ++ "]"
