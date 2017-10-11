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
import Data.List

import KernelGen ( genKernel )         -- module that generates OpenCL kernels, aka content of a .cl file
import FunctionTypes

main :: IO ()
main = do
  -- initialize OpenCL and print some information about it
  platforms <- clGetPlatformIDs
  hPutStrLn stdout $ "There are " ++ ((show . length) (platforms)) ++ " OpenCL platforms."
  vendor  <- ((oclPlatformInfo platforms) !! platformNumber)
  hPutStrLn stdout $ "Selected platform: [" ++ (show platformNumber) ++ "]" ++ vendor
  device <- head `fmap` clGetDeviceIDs (platforms!!platformNumber) CL_DEVICE_TYPE_GPU -- possible options: CPU, GPU, ALL, DEFAULT, ACCELERATOR
  deviceName <- clGetDeviceName device
  hPutStrLn stdout $ "Selected device: " ++ deviceName -- in my case Quadro K4200
  
  -- print generated OpenCL kernel
  hPutStrLn stdout ""
  hPutStrLn stdout $ "Using kernel:\n" ++ clText
  hPutStrLn stdout ""
  
  -- create context and compile OpenCL program
  context <- clCreateContext [CL_CONTEXT_PLATFORM (platforms!!platformNumber)] [device] $ hPutStrLn stdout -- uses "hPutStrLn stdout" to display text output(if something goes wrong)
  program <- clCreateProgramWithSource context clText -- creates OpenCL program from the kernel source
  clBuildProgram program [device] "" -- compiles the program without any specific optimization options ""
  kernel <- clCreateKernel program functionName -- function name in the source
  
  x1s <- newArray x1sData
  x2s <- newArray x2sData
  mem_x1s <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (vecSize, castPtr x1s)  
  mem_x2s <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (vecSize, castPtr x2s)  
  mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)
  clSetKernelArgSto kernel 0 mem_out  -- kernel's 0th agrument
  clSetKernelArgSto kernel 1 mem_x1s  -- kernel's 1st agrument
  clSetKernelArgSto kernel 2 mem_x2s  -- kernel's 2nd agrument
  
  -- create command queue and queue the execution on a device
  q <- clCreateCommandQueue context device [] -- creates command queue with default properties []
  eventExec <- clEnqueueNDRangeKernel q kernel [dataLength] [] []  -- queue the execution
  eventRead <- clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr x1s) [eventExec] -- read the result once execution is complete
  outputData <- peekArray dataLength x1s
  
  
  hPutStrLn stdout $ "Input array1 = " ++ show x1sData
  hPutStrLn stdout $ "Input array2 = " ++ show x2sData
  hPutStrLn stdout $ "Output array = " ++ show outputData
  return()
    where
      platformNumber = 0  -- using the first platform
      vecSize = dataLength * (sizeOf (0 :: CFloat))
      xsData = (take n [[x,x*2] | x <- [1..]]) :: [[CFloat]]
      x1sData = (transpose xsData)!!0
      x2sData = (transpose xsData)!!1
      dataLength = length xsData
      clText = genKernel testFunction
      functionName = name testFunction
      n = 10


-- replaces list of platfrom IDs with their vendors
oclPlatformInfo :: [CLPlatformID] -> [IO String]
oclPlatformInfo = map (\pid -> clGetPlatformInfo pid CL_PLATFORM_VENDOR)

-- TODO: multivariable and position independent function
testFunction = FunctionExpression "myAdd" "x1# + x2#" ["x1","x2"]

-- GOAL: the program should take something like this as an input and produce the result
testInput = "Integrate[1/(x^3 + 1)/(y^3 + 1), {x, 0, 1}, {y, 0, 1}]"
testNumOutput = showCReal 100 $ ((1/18) * (2 * sqrt(3) * pi + log(64))) ** 2 -- the exact number with 100 digits precision
testOutPut = "0.6983089976061547905950713595903295502322592708600975842346346477469051938999891540922414594979416232" -- the value of testNumOutput


{-
Major TODOs:
1) use function to generate function object from a string
2) use function to generate OpenCL kernel from a function object
3) use function to generate sequence of points
4) add metrics(time, precision)


TODO: Consider following improvements
1) Display all platforms
2) Allow the user to choose the platform
3) Display all devices
4) Allow to chose the device
5) Add class for kernel function with constructor
6) Wrap kernel compilation in a function
7) Monad for function construction
-}
