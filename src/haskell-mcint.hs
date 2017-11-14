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
import Foreign.C.Types( CDouble )
import Foreign.Ptr( Ptr )

import System.Process -- to run external program
import System.Environment
import System.Exit

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
  hFlush stdout
  end <- getCPUTime
  hPutStrLn stdout $ "Total execution time: " ++ (show $ (fromIntegral $ end - start) / 10^9)  ++ "ms"
  return ()

generateSobolSequence :: Int -> Int -> IO [[CDouble]]
generateSobolSequence n d = do
  -- initialize sobol sequence generator(external program that uses pre-calculated direction numbers)
  (_, Just hout, _, _) <- createProcess (proc "./src/sobol" [show n,show d,"./src/direction_numbers-joe-kuo-6.21201"]){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  sobolStr <- hGetContents hout
  return (toDataList sobolStr)
    where
      -- drop last symbol(extra new line...), divide output into lines, each line divide into words, and convert each word into Float
      toDataList = (map $ map (read :: String -> CDouble)) . (map words) . lines . init -- this is probably taking too much time and space



compute :: FunctionExpression -> IO ()
compute testFunction = do
  -- initialize OpenCL and print some information about it
  platforms <- clGetPlatformIDs
  myPrint $ "There are " ++ ((show . length) (platforms)) ++ " OpenCL platforms."
  vendor  <- ((oclPlatformInfo platforms) !! platformNumber)
  myPrint $ "Selected platform: [" ++ (show platformNumber) ++ "]" ++ vendor
  device <- head `fmap` clGetDeviceIDs (platforms!!platformNumber) CL_DEVICE_TYPE_DEFAULT -- possible options: CPU, GPU, ALL, DEFAULT, ACCELERATOR
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
  
  -- generate Sobol sequence data points
  xsPoints <- generateSobolSequence numOfPoints nD
  let xsData = transpose xsPoints

  -- allocate memory for input data and pass pointers to it to the kernel
  -- fist nD arguments are for input data(see KernelGen.hs)
  genStart <- getCPUTime -- actual generation happens here, since this is when xsData gets first used
  foldr1 (>>) [setKernelInputData context kernel (fromIntegral i) (xsData!!i) vecSize | i <- [0..nD-1]]
  genEnd <- getCPUTime

  -- allocate memory for output data(though we could reuse one of the pointers to the input data instead)
  out <- (mallocArray dataLength) :: IO (Ptr CDouble) -- this pointer is used later to retrieve the data from OpenCL device
  mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)
  clSetKernelArgSto kernel (fromIntegral nD) mem_out  -- kernel's last agrument is the output(see KernelGen.hs)
  
  -- create command queue and queue the execution on a device
  execStart <- getCPUTime
  q <- clCreateCommandQueue context device [] -- creates command queue with default properties []
  eventExec <- clEnqueueNDRangeKernel q kernel [dataLength] [] []  -- queue the execution
  eventRead <- clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr out) [eventExec] -- read the result once execution is complete
  outputData <- peekArray dataLength out
  execEnd <- getCPUTime
  
  -- print arrays
  --foldr1 (>>) [(myPrint $ "Input array" ++ (show i) ++ " = " ++ show (xsData!!i)) | i <- [0..nD-1]]
  --myPrint $ "Output array = " ++ show outputData
  
  -- calculate the sum of the output array, divide by number of points and print the result
  myPrint $ (++) (testString ++ " = ") $ show $ (foldl1' (+) outputData) / (fromIntegral numOfPoints)
  myPrint $ "Sequence generation time: " ++ (show $ (fromIntegral $ genEnd - genStart) / 10^9)  ++ "ms"
  myPrint $ "OCL execution and IO time: " ++ (show $ (fromIntegral $ execEnd - execStart) / 10^9)  ++ "ms"
  
  return()
    where
      platformNumber = 0  -- using the first platform
      numOfPoints = 2^20
      -- dementions of input data(length xsData) and the function(nD) should be equal!
      nD = length $ variables testFunction -- number of dimensions
      --xsPoints = transpose $ fmap ((gen1Dsec numOfPoints) . snd) $ variables testFunction -- xsPoints - list of points
      gen1Dsec :: Int -> Limits -> [CDouble]
      gen1Dsec nPoints (Limits l u) = [(l + (u-l)*(fromIntegral i)/(fromIntegral nPoints)) | i <- [1..nPoints]]
      --xsData = transpose xsPoints -- xsData - list of lists of point coordinates
      dataLength = numOfPoints
      vecSize = dataLength * (sizeOf (0 :: CDouble)) -- size of data transmitted to an OpenCL device
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

testString = "Integrate[1/(x1#*x1#*x1# + 1)/(x2#*x2#*x2# + 1), {x1, 0, 1}, {x2, 0, 1}]"

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
14) Revisit GPipe project for inspiration
15) Checkout accelerate(array operations on GPU)
16) Checkout shady-gen(another project for execution on GPU)

-}


{- Some choices
1) HOpenCL is not in a useful state (that package itself fails the tests)
2) OpenCLRAW seems to be too low level compared to OpenCL
3) No need for Criterion library. Basic metrics can be checked via prelude
   and on per function basis
4) 

-}
