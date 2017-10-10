{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module Main where

import System.IO
import Control.Parallel.OpenCL
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.Marshal.Array( newArray, peekArray )
import Foreign.C.Types( CFloat )

import Data.Number.CReal( showCReal )  -- arbitrary precision real numbers

import KernelGen ( genKernel )         -- module that generates OpenCL kernels, aka content of a .cl file
import FunctionTypes

main :: IO ()
main = do

--  kernel    <- openFile "kernel.cl" ReadMode      -- kernel is the file handle
--  clText    <- hGetContents kernel                -- clText contains contents of kernel as a String
  platforms <- clGetPlatformIDs
  hPutStrLn stdout $ "There are " ++ ((show . length) (platforms)) ++ " platforms, using:"
  vendor  <- ((oclPlatformInfo platforms) !! platformNumber)
  hPutStrLn stdout $ "[" ++ (show platformNumber) ++ "]" ++ vendor
  hPutStrLn stdout ""
  (device:_) <- clGetDeviceIDs (platforms!!platformNumber) CL_DEVICE_TYPE_GPU -- possible options: CPU, GPU, ALL, DEFAULT, ACCELERATOR
  deviceName <- clGetDeviceName device
  hPutStrLn stdout $ "Device: " ++ deviceName -- in my case Quadro K4200
  hPutStrLn stdout ""
  hPutStrLn stdout $ "Using kernel:\n" ++ clText
  hPutStrLn stdout ""
  context <- clCreateContext [CL_CONTEXT_PLATFORM (platforms!!platformNumber)] [device] print -- uses print to display text output(if something goes wrong)
  q <- clCreateCommandQueue context device [] -- creates command queue with default properties []
  program <- clCreateProgramWithSource context clText -- creates OpenCL program from the kernel source
  clBuildProgram program [device] "" -- compiles the program without any specific optimization options ""
  kernel0 <- clCreateKernel program functionName -- function name in the source

  input <- newArray inputData
  mem_in <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (vecSize, castPtr input)  
  mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)
  clSetKernelArgSto kernel0 0 mem_in  -- storable data 0
  clSetKernelArgSto kernel0 1 mem_out -- storable data 1
  eventExec <- clEnqueueNDRangeKernel q kernel0 [length inputData] [] []
  eventRead <- clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr input) [eventExec]
  
  outputData <- peekArray (length inputData) input
  hPutStrLn stdout $ "Input  array = " ++ show inputData
  hPutStrLn stdout $ "Output array = " ++ show outputData
  return()
    where
      platformNumber = 0  -- using the first platform
      vecSize = (length inputData) * (sizeOf (0 :: CFloat))
      inputData = 0.2:17.0:0.33:11:[] :: [CFloat]
      clText = genKernel testFunction
      functionName = name testFunction


-- replaces list of platfrom IDs with their vendors
oclPlatformInfo :: [CLPlatformID] -> [IO String]
oclPlatformInfo = map (\pid -> clGetPlatformInfo pid CL_PLATFORM_VENDOR)

testFunction = FunctionExpression "dupplicate" "2 * x1" ["x1"]

-- the program should take something like this as an input and produce the result
testInput = "Integrate[1/(x^3 + 1), {x, 0, 1}]"
testNumOutput = showCReal 100 $ (1/18) * (2 * sqrt(3) * pi + log(64)) -- the exact number with 100 digits precision
testOutPut = "0.8356488482647210533371034597001107667865221274843319432301883149605056010320161997633294384028262855" -- the value of testNumOutput


{-
Major TODOs:
1) use function to generate function object from a string
2) use function to generate OpenCL kernel from a function object
3) use function to generate sequence of points


TODO: Consider following improvements
Display all platforms
Allow the user to choose the platform
Display all devices
Allow to chose the device
Add class for kernel function with constructor
Wrap kernel compilation in a function
-}
