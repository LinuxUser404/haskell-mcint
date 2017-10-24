{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module Main where

import System.IO
import Control.Parallel.OpenCL
import Data.Number.CReal( showCReal )  -- arbitrary precision real numbers
import Data.List
import Text.ParserCombinators.Parsec

-- criterion looks like an overkill for our purposes... CPUTime is what I use instead
--import Criterion.Measurement -- for measuring performance
--import Criterion.Main -- benchmarks functions and prints execution statistics(min, max, average, standard deviation, etc)
import System.CPUTime


import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.Marshal.Array( newArray, peekArray, mallocArray )
import Foreign.C.Types( CFloat )
import Foreign.Ptr( Ptr )


import KernelGen ( genKernel )         -- module that generates OpenCL kernels, aka content of a .cl file
import FunctionTypes
import FunctionParser

-- TODO: consider switching to guards notation
main :: IO ()
main = do
  start <- getCPUTime
  case stringToFunction testString of
    Left  e -> (hPutStrLn stdout $ show e) -- if parser fails print the error
    Right func -> compute func             -- otherwise compute compute the input
  end <- getCPUTime
  hPutStrLn stdout $ "Total computation time: " ++ (show $ (fromIntegral $ end - start) / 10^9)  ++ "ms"
  return ()
  



compute :: FunctionExpression -> IO ()
compute testFunction = do
  -- initialize OpenCL and print some information about it
  platforms <- clGetPlatformIDs
  myPrint $ "There are " ++ ((show . length) (platforms)) ++ " OpenCL platforms."
  vendor  <- ((oclPlatformInfo platforms) !! platformNumber)
  myPrint $ "Selected platform: [" ++ (show platformNumber) ++ "]" ++ vendor
  device <- head `fmap` clGetDeviceIDs (platforms!!platformNumber) CL_DEVICE_TYPE_GPU -- possible options: CPU, GPU, ALL, DEFAULT, ACCELERATOR
  deviceName <- clGetDeviceName device
  myPrint $ "Selected device: " ++ deviceName -- in my case Quadro K4200
  
  -- print generated OpenCL kernel
  myPrint ""
  myPrint $ "Using kernel:\n" ++ clText
  myPrint ""
  
  -- create context and compile OpenCL program
  context <- clCreateContext [CL_CONTEXT_PLATFORM (platforms!!platformNumber)] [device] $ myPrint -- uses myPrint to display text output(if something goes wrong)
  program <- clCreateProgramWithSource context clText -- creates OpenCL program from the kernel source
  clBuildProgram program [device] "" -- compiles the program without any specific optimization options ""
  kernel <- clCreateKernel program functionName -- function name in the source
  
  -- allocate memory for input data and pass pointers to it to the kernel
  -- fist nD arguments are for input data(see KernelGen.hs)
  foldr1 (>>) [setKernelInputData context kernel (fromIntegral i) (xsData!!i) vecSize | i <- [0..nD-1]]

  -- allocate memory for output data(though we could reuse one of the pointers to the input data instead)
  out <- (mallocArray dataLength) :: IO (Ptr CFloat) -- this pointer is used later to retrieve the data from OpenCL device
  mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)
  clSetKernelArgSto kernel (fromIntegral nD) mem_out  -- kernel's last agrument is the output(see KernelGen.hs)
  
  -- create command queue and queue the execution on a device
  q <- clCreateCommandQueue context device [] -- creates command queue with default properties []
  eventExec <- clEnqueueNDRangeKernel q kernel [dataLength] [] []  -- queue the execution
  eventRead <- clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr out) [eventExec] -- read the result once execution is complete
  outputData <- peekArray dataLength out
  
  -- print arrays
  foldr1 (>>) [(myPrint $ "Input array" ++ (show i) ++ " = " ++ show (xsData!!i)) | i <- [0..nD-1]]
  myPrint $ "Output array = " ++ show outputData
  
  return()
    where
      platformNumber = 0  -- using the first platform
      numOfPoint = 10
      -- dementions of input data(length xsData) and the function(nD) should be equal!
      nD = length $ variables testFunction -- number of dimensions
      xsPoints = transpose $ fmap ((gen1Dsec numOfPoint) . snd) $ variables testFunction -- xsPoints - list of points
      gen1Dsec :: Int -> Limits -> [CFloat]
      gen1Dsec nPoints (Limits l u) = [(l + (u-l)*(fromIntegral i)/(fromIntegral nPoints)) | i <- [1..nPoints]]
      xsData = transpose xsPoints -- xsData - list of lists of point coordinates
      dataLength = length xsPoints
      vecSize = dataLength * (sizeOf (0 :: CFloat)) -- size of data transmitted to an OpenCL device
      clText = genKernel testFunction
      functionName = name testFunction
      myPrint = hPutStrLn stdout -- prints to stdout, can be easily modified to print to a file or a port
      createConstBuffer context' size' ptr' = clCreateBuffer context' [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (size', ptr')
      setKernelInputData context' kernel' argNum' data' dataSize' = ((newArray (data')) >>= (\xs -> (createConstBuffer context' dataSize' (castPtr xs)) >>= (\mem_xs -> clSetKernelArgSto kernel' argNum' mem_xs)))

-- replaces list of platfrom IDs with their vendors
oclPlatformInfo :: [CLPlatformID] -> [IO String]
oclPlatformInfo = map (\pid -> clGetPlatformInfo pid CL_PLATFORM_VENDOR)


stringToFunction :: String -> Either ParseError FunctionExpression
stringToFunction inputStr = parse parseIntegrate "Parse error" inputStr

testString = "Integrate[x1# + x2#, {x1, 0, 10}, {x2, 0, 20}]"

-- GOAL: the program should take something like this as an input and produce the result
testInput = "Integrate[1/(x^3 + 1)/(y^3 + 1), {x, 0, 1}, {y, 0, 1}]"
testNumOutput = showCReal 100 $ ((1/18) * (2 * sqrt(3) * pi + log(64))) ** 2 -- the exact number with 100 digits precision
testOutPut = "0.6983089976061547905950713595903295502322592708600975842346346477469051938999891540922414594979416232" -- the value of testNumOutput


{-
Next Steps:
1) Improve parser/genKernel. Get rid of '#' character in testString
2) Implement sobol sequence(might be unclear) or at least some sequence generator
3) Modify genKernel to perform integration
4) Use double precision

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
8) GPipe like function modifiers
9) Prebuild OpenCL binaries
10) kernel optimizations. Implement cl_khr_fp64 and read about others
11) Define a language to use with parser instead of hard-coding parser cases
12) Implement OpenCL data types. Data that exists only within an OpenCL device,
    like vertex and geometry shader instances of Num class in GPipe.
13) Think about OpenCL IO and other monads to guarantee type safety
14) Checkout hopencl and OpenCLRAW
-}
