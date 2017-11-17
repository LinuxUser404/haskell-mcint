{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
this module is not used
The program uses external solution (S. Joe and F. Y. Kuo 2008) sobol.cc for Sobol sequence generation
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module SequenceGen ( genSobolSec ) where 
--TODO
-- add optimization
-- current performance is too slow...

{-
This modules implements efficient construction of Sobol sequence following this paper:
Antonov, I.A. and Saleev, V.M. (1979) "An economic method of computing LPτ-sequences". Zh. Vych. Mat. Mat. Fiz. 19: 243–245 (in Russian); U.S.S.R. Comput. Maths. Math. Phys. 19: 252–256 (in English).

Russian version of the paper (original) is in public domain.


In short it is done in 3 steps:
1) Generate Gray code numbers
   g(i) = i xor (i div 2)
   g(i) = [1,3,2,6,7,5,4,12,13,15,..]
2) For each Gray code number compute the number of the bit that is different from previous Gray code number
   l(i) = 1 + log_2(g(i-1) xor g(i))
   l(i) = [1,2,1,3,1,2,1,4,1,2,..]
3) Compute Sobol sequence numbers using g(i), l(i) and direction numbers V(i,j)

Numbers V(i,j) are not specified in the paper by Antonov and Saleev. Those numbers are described in original Sobol paper:
Sobol, I. M. (1976) "Uniformly distributed sequences with an additional uniform property". Zh. Vych. Mat. Mat. Fiz. 16: 1332–1337 (in Russian); U.S.S.R. Comput. Maths. Math. Phys. 16: 236–242 (in English).

Russian version of the paper (original) is in public domain.

These numbers are somewhat random and have to satisfy some properties. The paper provides an example of these numbers computed
for dimensions up to 16 and l up to 19. So around 2^19 Sobol sequence numbers can be generated in 16 dimensions.



Notes:
Known ACM algorithms to generate Sobol sequences(all in fortran 77): 647, 659, 738
http://calgo.acm.org/647.gz -   41 dimensions
http://calgo.acm.org/659.gz - 1111 dimensions
http://calgo.acm.org/738.gz -   12 dimensions, MAXBASE=13
All of them use hard-coded tables of numbers used to generate the sequence.
http://web.maths.unsw.edu.au/~fkuo/sobol/index.html - S. Joe and F. Y. Kuo provided a table containing direction numbers up to 21001 demensions
but it is assumed higher dimensions have less impact on the function.

Non-sequential generation can be executed on an accelerator such as a modern GPU, FPGA, Xeon Phi, NVidia Tesla, etc
For sequential generation numbers l(i) can be efficiently generated independently from g(i) using haskell palindromic list constructor
Haskell laziness avoids computation of the same sequence multiple times. So it will be computed once and then reused for each integration.
If number of terms is known before computation the computation of the whole sequence will be performed at compile time.
If it is not known, but there is a reasonable guess haskell allows to specify the number of terms that should be pre-computed at compile time.

Numbers l(i) have a distinct palindromic pattern, so a more efficient way of using them might be a monad or a constructor. Here is the pattern:
1  l()  = [1]
2  l()  = [1,2,1]
3  l()  = [1,2,1,3,1,2,1]
4  l()  = [1,2,1,3,1,2,1,4,1,2,1,3,1,2,1]
...
n  l()  = (n-1 l())  ++ [n] ++  (n-1 l())
-}

import GHC.Integer.Logarithms
import Data.List
import Data.Bits

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U -- vectors for performance
import qualified Data.Vector.Storable as S -- vectors for performance


-- "g(i) = i xor (i div 2)"
grayCode :: Int -> Int
grayCode !i = xor i $ shift i (-1)

genSobolSec :: Int -> Int -> (U.Vector Double)
genSobolSec !n !d = (U.fromList) . concat $ (take n) (sobolSeq d)



-- "l(i) = 1 + log_2(g(i-1) xor g(i))"
l :: Int -> Int
l !i = (+) 1 $ countTrailingZeros $ xor (grayCode (i-1)) (grayCode i)
--ls 0 = []
--ls n = concat [(ls (n-1)),[n],(ls (n-1))]

{-
q(0,j) = 0
q(i,j) = q(i-1,j) xor V(l,j)
q(i,j) - j-th coordinate of i-th number in Sobol sequence
V(l,j) - j-th coordinate of l-th direction number
l - the number specified above

Direction numbers V have to be generated separately and depend on number of dimensions.
Sobol supplied a table for these numbers and properties they have to satisfy(so called A and A').
Unfortunately the numbers satisfying both properties are known maximum for 6 dimensions.
Numbers satisfying just property A known maximum for 16 dimensions.
Numbers satisfying property A supplied by other authors known up to 21001 dimensions(S. Joe and F. Y. Kuo 2008).
-}

-- direction numbers V by Sobol
--dNV dims n
dNV d n = (U.!) dNV_data ((d - 1) * dNV_MAXN + n - 1)
dNV_DIM = 16
dNV_MAXN = 19
dNV_data :: (U.Vector Int)
dNV_data = U.fromList
  [1, 1, 1,  1,  1,  1,   1,   1,   1,    1,    1,    1,     1,     1,     1,     1,      1,      1,      1,
   1, 3, 5, 15, 17, 51,  85, 255, 257,  771, 1285, 3855,  4369, 13107, 21845, 65535,  65537, 196611, 327685,
   1, 1, 7, 11, 13, 61,  67,  79, 465,  721,  823, 4091,  4125,  4141, 28723, 45311,  53505, 250113, 276231,
   1, 3, 7,  5,  7, 43,  49, 147, 439, 1013,  727,  987,  5889,  6915, 16647, 49925, 116487,  83243, 116529,
   1, 1, 5,  3, 15, 51, 125, 141, 177,  759,  267, 1839,  6929, 16241, 16565, 17139,  82207,  50979, 252717,
   1, 3, 1,  1,  9, 59,  25,  89, 321,  835,  833, 4033,  3913, 11643, 18777, 35225, 102401,  45059,  36865,
   1, 1, 3,  7, 31, 47, 109, 173, 181,  949,  471, 2515,  6211,  2147,  3169, 35873,  33841,  99889, 247315,
   1, 3, 3,  9,  9, 57,  43,  43, 225,  113, 1601,  579,  1731, 11977,  7241, 63609,  81003,  15595, 144417,
   1, 3, 7, 13,  3, 35,  89,   9, 235,  929, 1341, 3863,  1347,  4417,  5087, 12631, 103445, 152645, 130127,
   1, 1, 5, 11, 27, 53,  69,  25, 103,  615,  913,  977,  6197, 14651,  2507, 27109,   5205,  91369, 302231,
   1, 3, 5,  1, 15, 19, 113, 115, 411,  157, 1725, 3463,  2817,  9997,  7451, 12055,  44877,  24895, 508255,
   1, 1, 7,  3, 29, 51,  47,  97, 233,   39, 2021, 2909,  5459,  2615, 13329, 35887,  97323,  83101, 320901,
   1, 3, 7,  7, 21, 61,  55,  19,  59,  761, 1905, 3379,  8119, 13207,  8965,  9997,  75591, 226659, 187499,
   1, 1, 1,  9, 23, 37,  97,  97, 353,  169,  375, 1349,  5121, 13313, 19457,  1033,  62487, 250917, 234593,
   1, 3, 3,  5, 19, 33,   3, 197, 329,  983,  893, 3739,  7669,  2671, 18391, 31161,  12111, 259781,  36159,
   1, 1, 3, 13, 11,  7,  37, 101, 463,  657, 1599,  347,  2481,  5201,  3123, 32253,  78043,  63447, 508757]
{-
dNV 1  n = [1, 1, 1,  1,  1,  1,   1,   1,   1,    1,    1,    1,     1,     1,     1,     1,      1,      1,      1]!!(n-1)
dNV 2  n = [1, 3, 5, 15, 17, 51,  85, 255, 257,  771, 1285, 3855,  4369, 13107, 21845, 65535,  65537, 196611, 327685]!!(n-1)
dNV 3  n = [1, 1, 7, 11, 13, 61,  67,  79, 465,  721,  823, 4091,  4125,  4141, 28723, 45311,  53505, 250113, 276231]!!(n-1)
dNV 4  n = [1, 3, 7,  5,  7, 43,  49, 147, 439, 1013,  727,  987,  5889,  6915, 16647, 49925, 116487,  83243, 116529]!!(n-1)
dNV 5  n = [1, 1, 5,  3, 15, 51, 125, 141, 177,  759,  267, 1839,  6929, 16241, 16565, 17139,  82207,  50979, 252717]!!(n-1)
dNV 6  n = [1, 3, 1,  1,  9, 59,  25,  89, 321,  835,  833, 4033,  3913, 11643, 18777, 35225, 102401,  45059,  36865]!!(n-1)
dNV 7  n = [1, 1, 3,  7, 31, 47, 109, 173, 181,  949,  471, 2515,  6211,  2147,  3169, 35873,  33841,  99889, 247315]!!(n-1)
dNV 8  n = [1, 3, 3,  9,  9, 57,  43,  43, 225,  113, 1601,  579,  1731, 11977,  7241, 63609,  81003,  15595, 144417]!!(n-1)
dNV 9  n = [1, 3, 7, 13,  3, 35,  89,   9, 235,  929, 1341, 3863,  1347,  4417,  5087, 12631, 103445, 152645, 130127]!!(n-1)
dNV 10 n = [1, 1, 5, 11, 27, 53,  69,  25, 103,  615,  913,  977,  6197, 14651,  2507, 27109,   5205,  91369, 302231]!!(n-1)
dNV 11 n = [1, 3, 5,  1, 15, 19, 113, 115, 411,  157, 1725, 3463,  2817,  9997,  7451, 12055,  44877,  24895, 508255]!!(n-1)
dNV 12 n = [1, 1, 7,  3, 29, 51,  47,  97, 233,   39, 2021, 2909,  5459,  2615, 13329, 35887,  97323,  83101, 320901]!!(n-1)
dNV 13 n = [1, 3, 7,  7, 21, 61,  55,  19,  59,  761, 1905, 3379,  8119, 13207,  8965,  9997,  75591, 226659, 187499]!!(n-1)
dNV 14 n = [1, 1, 1,  9, 23, 37,  97,  97, 353,  169,  375, 1349,  5121, 13313, 19457,  1033,  62487, 250917, 234593]!!(n-1)
dNV 15 n = [1, 3, 3,  5, 19, 33,   3, 197, 329,  983,  893, 3739,  7669,  2671, 18391, 31161,  12111, 259781,  36159]!!(n-1)
dNV 16 n = [1, 1, 3, 13, 11,  7,  37, 101, 463,  657, 1599,  347,  2481,  5201,  3123, 32253,  78043,  63447, 508757]!!(n-1)
-}


--sobolSequence
-- qij - j-th coordinate of i-th number in Sobol squence
qij :: Int -> Int -> Int
qij !i !j = (U.!) (q_data!!i) (j-1)
q_data :: [U.Vector Int]
q_data = map snd $ (iterate f) (0, (U.fromList) (replicate 16 0))
  where
    f (i, v) = (i + 1, U.fromList (map (\j -> xor ((U.!) v (j-1)) (dNV j (l (i+1)))) [1..16] ))

sobolSeq :: Int -> [[Double]]
sobolSeq d = [fmap (\j -> (fromIntegral $ qij i j) / (fromIntegral $ iPow2 i)) [1..d] | i <- [1..]]

--iPow2 i - smallest power of 2 grater than i 
--iPow2 i = bit $ ceiling $ logBase 2 i
iPow2 :: Int -> Int
iPow2 i = head $ filter (> i) powersOf2

powersOf2 = iterate (* 2) 1

-- Pi is 6 times volume of 1/8 of a unit sphere
--testPi 2000 = 3.699, takes few seconds
--testPi 10000 = 3.0822, takes few minutes
--needs optimization
testPi n = (6.0) * (fromIntegral . length $ filter testInsideASphere (take n (sobolSeq 3))) / (fromIntegral n)
testInsideASphere [a,b,c] = a*a + b*b + c*c < 1
