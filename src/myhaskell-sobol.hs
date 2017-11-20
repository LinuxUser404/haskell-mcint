{-# LANGUAGE BangPatterns #-}
import System.IO
import Data.List
import qualified Data.Sequence as Seq
import Data.Bits
import Text.ParserCombinators.Parsec

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U -- vectors for performance
import qualified Data.Vector.Storable as S -- vectors for performance

import GSL.Random.Quasi

import System.Process( createProcess, std_err, std_in, std_out, StdStream( CreatePipe ), proc ) -- to run external program

dNFilePath = "./direction_numbers-joe-kuo-6.21201"

main :: IO ()
main = do
  hInput <- openFile dNFilePath ReadMode
  _ <- hGetLine hInput
  mycon <- hGetContents hInput
  hPutStrLn stdout $ show $ highDSobolGen mycon 15 2
  return ()

-- High dimensional Sobol sequence generation
-- highDSobolGen 15 2 = [0.0,0.0,0.5,0.5,0.75,0.25,0.25,0.75,0.375,0.375,0.875,0.875,0.625,0.125,0.125,0.625,0.1875,0.3125,0.6875,0.8125,0.9375,6.25e-2,0.4375,0.5625,0.3125,0.1875,0.8125,0.6875,0.5625,0.4375]
tempFunction :: Int -> Int -> IO (U.Vector Double)
tempFunction n d = do
  (_, Just hout, _, _) <- createProcess (proc "./sobol" [show n,show d, dNFilePath]){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  sobolStr <- hGetContents hout
  return ((mystringToListOfPoints n d) sobolStr)
  where
    mystringToListOfPoints !n !d s = (U.fromListN (n * d)) . (map read) . words $ s
    
highDSobolGen :: String -> Int -> Int -> (U.Vector Double)
highDSobolGen inStr n d = U.fromList $ map fromIntegral myC
  where
    dirNumDataLines = take 21200 $ lines inStr
    myL :: Int
    myL = ceiling (log(fromIntegral n)/log(2.0))
    myC :: [Int]
    myC = map findFirstZero [0..n-1] -- the l in old file
    findFirstZero = ((+) 1) . countTrailingZeros . ((+) 1)
--    myPoints :: [[Double]]
--    myPoints = iterate myIterator $ replicate d 0.0

gslSobolGen :: Int -> Int -> IO (U.Vector Double)
gslSobolGen n d = do
  rng <- newQRNG sobol d
  lst <- sequence (replicate n (getListSample rng))
  return(U.concat $ map U.fromList lst)






















 
