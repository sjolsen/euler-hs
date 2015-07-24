module Euler0017 where

import Text.Numeral.Language.EN
import Text.Numeral.Grammar.Reified (defaultInflection)
import qualified Data.Text as Text
import Data.Char

english :: Integer -> Maybe Text.Text
english = gb_cardinal defaultInflection

letters :: Integer -> Integer
letters = toInteger . maybe 0 countLetters . english
  where countLetters = Text.length . Text.filter isAlpha

solution = sum . fmap letters $ [1..1000]
