{-# LANGUAGE BangPatterns #-}
module SobolGen(highDSobolGenIO) where

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

dNFilePath = "./src/direction_numbers-joe-kuo-6.21201"

highDSobolGenIO :: Int -> Int -> IO (S.Vector Double)
highDSobolGenIO n d = do
  hInput <- openFile dNFilePath ReadMode
  _ <- hGetLine hInput
  mycon <- hGetContents hInput
  return (highDSobolGen mycon n d)

-- High dimensional Sobol sequence generation
tempFunction :: Int -> Int -> IO (U.Vector Double)
tempFunction n d = do
  (_, Just hout, _, _) <- createProcess (proc "./sobol" [show n,show d, dNFilePath]){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  sobolStr <- hGetContents hout
  return ((mystringToListOfPoints n d) sobolStr)
  where
    mystringToListOfPoints !n !d s = (U.fromListN (n * d)) . (map read) . words $ s
    
highDSobolGen :: String -> Int -> Int -> (S.Vector Double)
highDSobolGen inStr n d = S.fromListN (n*d) $ map ((/(2.0^32)) . fromIntegral) myXs
  where
    myXs = [myXij i j | i <- [0..n-1], j <- [0..d-1]]
    myXij :: Int -> Int -> Int
    myXij i j
      | i == 0    = 0
      | otherwise = (myXij (i-1) j) `xor` ((U.!) ((!!) myV j) ((U.!) myC (i-1)))
--    dirNumDataLines = take 21200 $ lines inStr
    dirNumDataLines = take (d-1) $ lines inStr
    myL :: Int
    myL = ceiling (log(fromIntegral n)/log(2.0))
    myC :: (U.Vector Int)
    myC = U.fromListN n $ map findFirstZero [1..n] -- the (l-1) in old file
    findFirstZero = countTrailingZeros
    myV :: [U.Vector Int]
    myV = take d $ (U.fromListN myL [2^(31-i) | i <- [0..myL-1]]):(map (myVj . (map read) . words) dirNumDataLines)
    myVj :: [Int] -> (U.Vector Int)
    myVj (_:s:a:mi) = U.fromListN myL $ genDirectionNumbers s a mi

gslSobolGen :: Int -> Int -> IO (U.Vector Double)
gslSobolGen n d = do
  rng <- newQRNG sobol d
  lst <- sequence (replicate n (getListSample rng))
  return(U.concat $ map U.fromList lst)

genDirectionNumbers :: Int -> Int -> [Int] -> [Int]
genDirectionNumbers s a ms = listV
  where
    listV = [myVi i | i <- [0..]]
    myVi :: Int -> Int
    myVi i
      | i < s     = unsafeShiftL (ms!!i) (31-i)
      | otherwise = (myVi (i-s)) `xor` ((myVi (i-s)) `div` (2 ^ s)) `xor` (prevVs i a)
    prevVs :: Int -> Int -> Int
    prevVs i' a' = foldl (xor) 0 [(( unsafeShiftR a' $ (s-k-1)) `mod` 2) * (myVi (i'-k)) | k <- [1..s-1]]
