module Main where

import Data.Maybe

main :: IO ()
main = putStrLn $ show $ length $ passwordsInRange 245182 790572

passwordsInRange :: Int -> Int -> [String]
passwordsInRange lower upper =
  takeWhile (\n -> read n <= upper) $
    dropWhile (\n -> read n < lower) genPasswords

genPasswords :: [String]
genPasswords = gen 6 Nothing False

-- Generate all valid passwords of given length.
-- If `hasRepeat` then the repeated adjacent digit requirement has been met.
gen :: Int -> Maybe Int -> Bool -> [String]
gen n prevDigit hasRepeat =
  if n == 0 then
    if hasRepeat then [""] else []
  else
    let startDigit = fromMaybe 0 prevDigit
        digitRange = [startDigit..9]
    in flip concatMap digitRange $ \digit1 ->
          map (show digit1 ++) (gen (n - 1) (Just digit1) (hasRepeat || prevDigit == Just digit1))
