module Main where

import System.Console.ANSI
import System.Exit

-- The solutions proper

import qualified Euler0001
import qualified Euler0002
import qualified Euler0011
import qualified Euler0012

type Solution = (Integer, Integer, Integer)

solutions :: [Solution]
solutions =
  [ (1,  Euler0001.solution, 233168)
  , (2,  Euler0002.solution, 4613732)
  , (11, Euler0011.solution, 70600674)
  , (12, Euler0012.solution, 76576500)
  ]

-- Test driver

padLeft :: Int -> Char -> String -> String
padLeft n c s
  | length s < n = replicate (n - length s) c ++ s
  | otherwise    = s

checkSolution :: Solution -> IO Bool
checkSolution (n, actual, expected)
  | actual == expected = do
      putStr $ padLeft 4 ' ' (show n) ++ ": "
      withColor Dull Green $ putStrLn $ show actual
      return True
  | otherwise = do
      putStr $ padLeft 4 ' ' (show n) ++ ": "
      withColor Vivid Red $ putStr (show actual)
      putStrLn $ " (expected " ++ (show expected) ++ ")"
      return False

withColor :: ColorIntensity -> Color -> IO a -> IO a
withColor i c x = do
  setSGR [SetColor Foreground i c]
  r <- x
  setSGR []
  return r

main :: IO ()
main = do
  results <- mapM checkSolution solutions
  if (all id results)
    then exitSuccess
    else exitFailure
