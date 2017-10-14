{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Monte-Carlo integration with OpenCL in Haskell
-}
module SequenceGen where 
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

import Data.List
import Data.Bits


-- "g(i) = i xor (i div 2)"
grayCode :: Int -> Int
grayCode i = xor i $ shift i (-1)

-- "l(i) = 1 + log_2(g(i-1) xor g(i))"
-- l i = (+) 1 $ countTrailingZeros $ xor (grayCode (i-1)) (grayCode i)
ls 0 = []
ls n = concat [(ls (n-1)),[n],(ls (n-1))]

{-
q(0,j) = 0
q(i,j) = q(i-1,j) xor V(l,j)
q(i,j) - j-th coordinate of i-th number in Sobol sequence
V(l,j) - j-th coordinate of l-th direction number
l - the number specified above

Direction numbers V have to be generated separately and depend on number of dimensions.
Sobol supplied a table for these numbers and properties they have to satisfy(so called A and A').
Unfortunately the numbers satisfying both properties are known maximum for 6 dimensions.
Numbers satisfying just property A known maximum 16 dimensions.
-}

-- direction numbers V
--dNV dims n
dNV 1  n = [1, 1, 1,  1,  1,  1,   1,   1,   1,   1,    1,    1,    1,     1,     1,     1,     1,      1,      1]!!(n-1)
dNV 2  n = [1, 3, 5, 15, 17, 51,  85, 255, 257, 771, 1285, 3855, 4369, 13107, 21845, 65535, 65537, 196611, 327685]!!(n-1)
--TODO: enter the table from the paper
--dNV 3  n = [1, 1, 7, 11, 13, 61,  67, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 4  n = [1, 3, 7,  5,  7, 43,  49, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 5  n = [1, 1, 5,  3, 15, 51, 125, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 6  n = [1, 3, 1,  1,  9, 59,  25, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 7  n = [1, 1, 3,  7, 31, 47, 109, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 8  n = [1, 3, 3,  9,  9, 57,  43, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 9  n = [1, 3, 7, 13,  3, 35,  89, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 10 n = [1, 1, 5, 11, 27, 53,  69, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 11 n = [1, 3, 5,  1, 15, 19, 113, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 12 n = [1, 1, 7,  3, 29, 51,  47, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 13 n = [1, 3, 7,  7, 21, 61,  55, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 14 n = [1, 1, 1,  9, 23, 37,  97, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 15 n = [1, 3, 3,  5, 19, 33,   3, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)
--dNV 16 n = [1, 1, 3, 13, 11,  7,  37, 1,1,1,1,1,1,1,1,1,1,1,1]!!(n-1)

--sobolSequence




