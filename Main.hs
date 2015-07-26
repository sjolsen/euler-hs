{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.ANSI
import System.Exit
import System.Environment
import Control.Exception
import Data.Typeable
import Data.List

-- The solutions proper

import qualified Euler0001
import qualified Euler0002
import qualified Euler0003
import qualified Euler0004
import qualified Euler0005
import qualified Euler0006
import qualified Euler0007
import qualified Euler0008
import qualified Euler0009
import qualified Euler0010
import qualified Euler0011
import qualified Euler0012
import qualified Euler0013
import qualified Euler0014
import qualified Euler0015
import qualified Euler0016
import qualified Euler0017
import qualified Euler0018
import qualified Euler0019
import qualified Euler0020
import qualified Euler0021
import qualified Euler0022
import qualified Euler0023
import qualified Euler0024
import qualified Euler0025
import qualified Euler0026
import qualified Euler0027
import qualified Euler0028
import qualified Euler0029
import qualified Euler0030
import qualified Euler0031
import qualified Euler0032
import qualified Euler0033
import qualified Euler0034
import qualified Euler0035
import qualified Euler0036
import qualified Euler0037
import qualified Euler0038
import qualified Euler0039
import qualified Euler0040
import qualified Euler0041
import qualified Euler0042
import qualified Euler0043
import qualified Euler0044
import qualified Euler0045
import qualified Euler0046
import qualified Euler0047
import qualified Euler0048
import qualified Euler0049
import qualified Euler0050
import qualified Euler0051
import qualified Euler0052
import qualified Euler0053
import qualified Euler0054
import qualified Euler0055
import qualified Euler0056
import qualified Euler0057
import qualified Euler0058
import qualified Euler0059
import qualified Euler0060
import qualified Euler0061
import qualified Euler0062
import qualified Euler0063
import qualified Euler0064
import qualified Euler0065
import qualified Euler0066
import qualified Euler0067
import qualified Euler0068
import qualified Euler0069
import qualified Euler0070
import qualified Euler0071
import qualified Euler0072
import qualified Euler0073
import qualified Euler0074
import qualified Euler0075
import qualified Euler0076
import qualified Euler0077
import qualified Euler0078
import qualified Euler0079
import qualified Euler0080
import qualified Euler0081
import qualified Euler0082
import qualified Euler0083
import qualified Euler0084
import qualified Euler0085
import qualified Euler0086
import qualified Euler0087
import qualified Euler0088
import qualified Euler0089
import qualified Euler0090
import qualified Euler0091
import qualified Euler0092
import qualified Euler0093
import qualified Euler0094
import qualified Euler0095
import qualified Euler0096
import qualified Euler0097
import qualified Euler0098
import qualified Euler0099

type Solution = (Integer, IO Integer, Integer)

solutions :: [Solution]
solutions =
  [ (1,  Euler0001.solution, 233168)
  , (2,  Euler0002.solution, 4613732)
  , (3,  Euler0003.solution, 6857)
  , (4,  Euler0004.solution, 906609)
  , (5,  Euler0005.solution, 232792560)
  , (6,  Euler0006.solution, 25164150)
  , (7,  Euler0007.solution, 104743)
  , (8,  Euler0008.solution, 23514624000)
  , (9,  Euler0009.solution, 31875000)
  , (10, Euler0010.solution, 142913828922)
  , (11, Euler0011.solution, 70600674)
  , (12, Euler0012.solution, 76576500)
  , (13, Euler0013.solution, 5537376230)
  , (14, Euler0014.solution, 837799)
  , (15, Euler0015.solution, 137846528820)
  , (16, Euler0016.solution, 1366)
  , (17, Euler0017.solution, 21124)
  , (18, Euler0018.solution, 1074)
  , (19, Euler0019.solution, 171)
  , (20, Euler0020.solution, 648)
  , (21, Euler0021.solution, 31626)
  , (22, Euler0022.solution, 871198282)
  , (23, Euler0023.solution, 4179871)
  , (24, Euler0024.solution, 2783915460)
  , (25, Euler0025.solution, 4782)
  , (26, Euler0026.solution, 983)
  , (27, Euler0027.solution, -59231)
  , (28, Euler0028.solution, 669171001)
  , (29, Euler0029.solution, 9183)
  , (30, Euler0030.solution, 443839)
  , (31, Euler0031.solution, 73682)
  , (32, Euler0032.solution, 45228)
--  , (33, Euler0033.solution, 0)
--  , (34, Euler0034.solution, 0)
--  , (35, Euler0035.solution, 0)
--  , (36, Euler0036.solution, 0)
--  , (37, Euler0037.solution, 0)
--  , (38, Euler0038.solution, 0)
--  , (39, Euler0039.solution, 0)
--  , (40, Euler0040.solution, 0)
--  , (41, Euler0041.solution, 0)
--  , (42, Euler0042.solution, 0)
--  , (43, Euler0043.solution, 0)
--  , (44, Euler0044.solution, 0)
--  , (45, Euler0045.solution, 0)
--  , (46, Euler0046.solution, 0)
--  , (47, Euler0047.solution, 0)
--  , (48, Euler0048.solution, 0)
--  , (49, Euler0049.solution, 0)
--  , (50, Euler0050.solution, 0)
--  , (51, Euler0051.solution, 0)
--  , (52, Euler0052.solution, 0)
--  , (53, Euler0053.solution, 0)
--  , (54, Euler0054.solution, 0)
--  , (55, Euler0055.solution, 0)
--  , (56, Euler0056.solution, 0)
--  , (57, Euler0057.solution, 0)
--  , (58, Euler0058.solution, 0)
--  , (59, Euler0059.solution, 0)
--  , (60, Euler0060.solution, 0)
--  , (61, Euler0061.solution, 0)
--  , (62, Euler0062.solution, 0)
--  , (63, Euler0063.solution, 0)
--  , (64, Euler0064.solution, 0)
--  , (65, Euler0065.solution, 0)
--  , (66, Euler0066.solution, 0)
--  , (67, Euler0067.solution, 0)
--  , (68, Euler0068.solution, 0)
--  , (69, Euler0069.solution, 0)
--  , (70, Euler0070.solution, 0)
--  , (71, Euler0071.solution, 0)
--  , (72, Euler0072.solution, 0)
--  , (73, Euler0073.solution, 0)
--  , (74, Euler0074.solution, 0)
--  , (75, Euler0075.solution, 0)
--  , (76, Euler0076.solution, 0)
--  , (77, Euler0077.solution, 0)
--  , (78, Euler0078.solution, 0)
--  , (79, Euler0079.solution, 0)
--  , (80, Euler0080.solution, 0)
--  , (81, Euler0081.solution, 0)
--  , (82, Euler0082.solution, 0)
--  , (83, Euler0083.solution, 0)
--  , (84, Euler0084.solution, 0)
--  , (85, Euler0085.solution, 0)
--  , (86, Euler0086.solution, 0)
--  , (87, Euler0087.solution, 0)
--  , (88, Euler0088.solution, 0)
--  , (89, Euler0089.solution, 0)
--  , (90, Euler0090.solution, 0)
--  , (91, Euler0091.solution, 0)
--  , (92, Euler0092.solution, 0)
--  , (93, Euler0093.solution, 0)
--  , (94, Euler0094.solution, 0)
--  , (95, Euler0095.solution, 0)
--  , (96, Euler0096.solution, 0)
--  , (97, Euler0097.solution, 0)
--  , (98, Euler0098.solution, 0)
--  , (99, Euler0099.solution, 0)
  ]

-- Test driver

padLeft :: Int -> Char -> String -> String
padLeft n c s
  | length s < n = replicate (n - length s) c ++ s
  | otherwise    = s

checkSolution :: Solution -> IO Bool
checkSolution (n, s, expected) = do
  actual <- s
  putStr $ padLeft 4 ' ' (show n) ++ ": "
  if actual == expected
    then do
      withColor Dull Green $ putStrLn (show actual)
      return True
    else do
      withColor Vivid Red $ putStr (show actual)
      putStrLn $ " (expected " ++ (show expected) ++ ")"
      return False

withColor :: ColorIntensity -> Color -> IO a -> IO a
withColor i c x = do
  setSGR [SetColor Foreground i c]
  r <- x
  setSGR []
  return r

data UndefinedProblemException = UPE Integer
  deriving (Typeable)

instance Exception UndefinedProblemException

instance Show UndefinedProblemException where
  show (UPE x) = ("Problem " ++ show x ++ " not defined")

selectTests :: [String] -> [Solution]
selectTests []   = solutions
selectTests args =
  let nums = sort $ fmap read args in
  rightIntersect nums solutions
  where
    cmp x (y, _, _) = compare x y

    rightIntersect :: [Integer] -> [Solution] -> [Solution]
    rightIntersect []     _      = []
    rightIntersect (x:_)  []     = throw (UPE x)
    rightIntersect (x:xs) (y:ys) = case cmp x y of
      LT -> throw (UPE x)
      EQ -> y : rightIntersect xs ys
      GT -> rightIntersect (x:xs) ys

main :: IO ()
main = do
  args <- getArgs
  let tests = selectTests args
  results <- mapM checkSolution tests
  if (all id results)
    then exitSuccess
    else exitFailure
