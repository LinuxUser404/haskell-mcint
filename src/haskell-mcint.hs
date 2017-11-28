{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
--module Main where
{-# LANGUAGE BangPatterns #-} -- for strictness
--{-# ForeignFunctionInterface #-}
-- {-# MagicHash #-} -- for using raw unboxed types

import System.IO
import Control.Parallel.OpenCL
--import Data.Number.CReal( showCReal )  -- arbitrary precision real numbers
import Data.List
import Text.ParserCombinators.Parsec
import GSL.Random.Quasi -- GLS implementation of Sobol sequence (up to 40 dimensions)

--import qualified Data.Sequence as Seq -- to sequence IO actions
--import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U -- vectors of unboxed types - best performance
import qualified Data.Vector.Storable as S -- vectors of storable types - best for FFI
--import qualified Data.Vector.Storable.Internal as SI

-- criterion looks like an overkill for our purposes... CPUTime is what I use instead
--import Criterion.Measurement -- for measuring performance
--import Criterion.Main -- benchmarks functions and prints execution statistics(min, max, average, standard deviation, etc)
import System.CPUTime

--import GHC.Prim -- raw unboxed types

import Foreign( castPtr, nullPtr, sizeOf, allocaArray )
import Foreign.Marshal.Array( peekArray, mallocArray, advancePtr )
import Foreign.Ptr( Ptr )
import Foreign.C
import Foreign.ForeignPtr.Unsafe( unsafeForeignPtrToPtr )

import KernelGen( genKernel )         -- module that generates OpenCL kernels, aka content of a .cl file
import FunctionTypes
import FunctionParser

foreign import ccall "sobseq.h sobseq"
  cSobolSeq :: Ptr (Double) -> Int -> Int -> IO ()

-- TODO: consider switching to guards notation
main :: IO ()
main = do
  start <- getCPUTime
  case parse parseIntegrate "Parse error" testString of
    Left  e -> (hPutStrLn stdout $ show e) -- if parser fails print the error
    Right func -> compute func             -- otherwise compute compute the input
  hFlush stdout
  end <- getCPUTime
  hPutStrLn stdout $ "Total execution time: " ++ (show $ (fromIntegral $ end - start) / 10^9)  ++ "ms"
  return ()
{-
cSobolGen :: Int -> Int -> IO (S.Vector Double)
cSobolGen n d = do
  myPtr <- (mallocArray d) :: IO (Ptr Double)
  cSobolSeq myPtr d 0
  return()
-}


fastSobolGen :: Int -> Int -> IO (S.Vector Double)
fastSobolGen n d = do
  ptr <- mallocArray (n*d)
--  sequence (map (\i -> cSobolSeq (advancePtr ptr (i*d)) d i) [0..n-1])
  foldr1 (>>) (map (\i -> cSobolSeq (advancePtr ptr (i*d)) d i) [0..n-1]) -- more than 10 times faster than sequence
  lst <- peekArray (n*d) ptr
  return(S.fromListN (n*d) lst)

gslSobolGen :: Int -> Int -> IO (S.Vector Double)
gslSobolGen n d = do
  rng <- newQRNG sobol d
  lst <- sequence (replicate n (getListSample rng))
  return(S.fromListN (n*d) $ concat lst)

sobolGen :: Int -> Int -> IO (S.Vector Double)
sobolGen = fastSobolGen

strToDouble :: String -> Double
strToDouble !s = read s

mystringToListOfPoints :: Int -> Int -> String -> (U.Vector Double)
mystringToListOfPoints !n !d s = (U.fromListN (n * d)) . (map strToDouble) . words $ s

--myTranspose :: Int -> Int -> (S.Vector Double) -> [S.Vector Double]
--myTranspose !n !d !v = map (\i -> S.fromList (map (\j -> (S.unsafeIndex) v (j * d + i) ) [0..n-1] )) [0..d-1]

unsafeStorableVectorToPtr = castPtr . unsafeForeignPtrToPtr . fst . (S.unsafeToForeignPtr0)

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
  genStart <- getCPUTime -- actual generation happens here, since this is when xsData gets first used
  xsPoints <- sobolGen numOfPoints nD

  -- allocate memory for input data and pass pointers to it to the kernel
  setKernelInputData context kernel 0 xsPoints -- 0th argument is the input
  genEnd <- getCPUTime

  -- allocate memory for output data(though we could reuse one of the pointers to the input data instead)
  out <- (mallocArray dataLength) :: IO (Ptr Double) -- this pointer is used later to retrieve the data from OpenCL device
  mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)
  clSetKernelArgSto kernel (fromIntegral 1) mem_out  -- kernel's last agrument(1) is the output(see KernelGen.hs)
  
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
  myPrint $ (++) (testString ++ " = ") $ show $ (foldl1' (+) outputData) / (fromIntegral numOfPoints) -- testIntegration
--  myPrint $ (++) ("Pi = ") $ show $ ((*) 6.0 $ fromIntegral $ length $ filter (<1.0) outputData) / (fromIntegral numOfPoints) -- testPi
  myPrint $ "Sequence generation time: " ++ (show $ (fromIntegral $ genEnd - genStart) / 10^9)  ++ "ms"
--  myPrint $ "Sequence transposition time: " ++ (show $ (fromIntegral $ transEnd - transStart) / 10^9)  ++ "ms"
  myPrint $ "OCL execution and IO time: " ++ (show $ (fromIntegral $ execEnd - execStart) / 10^9)  ++ "ms"
  
  return()
    where
      platformNumber = 0 :: Int  -- using the first platform
      numOfPoints = ((2^20) :: Int) -- 2^27 takes a bit less than 32 GiB of memory
      -- dementions of input data(length xsData) and the function(nD) should be equal!
      nD = length $ variables testFunction -- number of dimensions
      dataLength = numOfPoints
      vecSize = dataLength * (sizeOf (undefined :: Double)) -- size of data transmitted to an OpenCL device
--      clText = genKernel testFunction
      clText = testCLtext
      functionName = name testFunction
      myPrint = hPutStrLn stdout -- prints to stdout, can be easily modified to print to a file or a port
      createConstBuffer context' size' ptr' = clCreateBuffer context' [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (size', ptr')
      setKernelInputData context' kernel' argNum' vec = (createConstBuffer context' ((S.length vec) * (sizeOf (undefined :: Double))) (unsafeStorableVectorToPtr vec)) >>= (\mem_xs -> clSetKernelArgSto kernel' argNum' mem_xs)
    
-- replaces list of platfrom IDs with their vendors
oclPlatformInfo :: [CLPlatformID] -> [IO String]
oclPlatformInfo = map (\pid -> clGetPlatformInfo pid CL_PLATFORM_VENDOR)



testString :: String
testString = "Integrate[1/(x1#*x1#*x1# + 1)/(x2#*x2#*x2# + 1), {x1, 0.0, 1.0}, {x2, 0.0, 1.0}]" -- testIntegration
--testString = "Integrate[exp((x#*x# + 1) * log(x#)), {x, -1000, exp(20)}]" -- testIntegration
--testString = "Integrate[x1#*x1# + x2#*x2# + x3#*x3#, {x1, 0, 1}, {x2, 0, 1}, {x3, 0, 1}]" -- testPi

-- GOAL: the program should take something like this as an input and produce the result
--testInput :: String
--testInput = "Integrate[1/(x^3 + 1)/(y^3 + 1), {x, 0, 1}, {y, 0, 1}]"
--testNumOutput :: String
--testNumOutput = showCReal 100 $ ((1/18) * (2 * sqrt(3) * pi + log(64))) ** 2 -- the exact number with 100 digits precision
--testOutPut :: String
--testOutPut = "0.6983089976061547905950713595903295502322592708600975842346346477469051938999891540922414594979416232" -- the value of testNumOutput
testCLtext = "__kernel void Integrate(__global double *x, __global double *out){int id = get_global_id(0);out[id] = 1/(x[id]*x[id]*x[id] + 1)/(x[id+1]*x[id+1]*x[id+1] + 1);}"

{-
Next Steps:
1) Improve parser/genKernel. Get rid of '#' character in testString and allow pointer arithmetic in the kernel
2) Pass packed array to GPU instead of doing intermediate transposition
3) Modify genKernel to perform summation in parallel on GPU
4) Make a haskell version of Joe and Kuo Sobol sequence generator

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
17) Sobol sequence generation by GSL (GNU Scientific Library)
18) Pass huge chunks of memory to GPU(streaming textures?), 32GiB seems to be fine
-}


{- Some choices
1) HOpenCL is not in a useful state (that package itself fails the tests)
2) OpenCLRAW seems to be too low level compared to OpenCL
3) No need for Criterion library. Basic metrics can be checked via prelude
   and on per function basis
4) (S. Joe and F. Y. Kuo 2008) Sobol sequence generation solution is too slow, plus using it requires type conversions...
5) fromListN is more efficient than (force . fromList)
-}
