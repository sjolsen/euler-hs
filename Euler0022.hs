module Euler0022 where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Error
import Data.Char
import Data.List
import Data.Maybe

cvalue :: Integral a => Char -> Maybe a
cvalue c
  | isAlpha c = Just . fromIntegral $ 1 + ord (toUpper c) - ord 'A'
  | otherwise = Nothing

value :: Integral a => String -> a
value = sum . catMaybes . fmap cvalue

solve :: [String] -> Integer
solve names = sum (zipWith (*) (fmap value $ sort names) [1..])

solution :: IO Integer
solution = do
  input <- readFile fileName
  let names = parse parser fileName input
  case names of
    Left e -> do
      putStrLn "Error:"
      mapM (putStrLn . ("  " ++) . messageString) $ errorMessages e
      return (-1)
    Right n -> do
      return (solve n)
  where
    fileName = "data/p022_names.txt"
    parser   = stringLiteral lexer `sepBy` comma lexer
    lexer    = haskell
