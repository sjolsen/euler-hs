module Main where

import System.Console.ANSI
import System.Exit

-- The solutions proper

import qualified Euler11
import qualified Euler12

type Solution = (Integer, Integer, Integer)

solutions :: [Solution]
solutions =
  [ (11, Euler11.solution, 70600674)
  , (12, Euler12.solution, 76576500)
  ]

-- Test driver

checkSolution :: Solution -> IO Bool
checkSolution (n, actual, expected)
  | actual == expected = do
      putStr $ (show n) ++ ": "
      withColor Dull Green $ putStrLn $ show actual
      return True
  | otherwise = do
      putStr $ (show n) ++ ": "
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
