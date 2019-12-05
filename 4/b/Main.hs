module Main where

import Data.Maybe

main :: IO ()
main = putStrLn $ show $ length $ passwordsInRange 245182 790572

passwordsInRange :: Int -> Int -> [String]
passwordsInRange lower upper =
  takeWhile (\n -> read n <= upper) $
    dropWhile (\n -> read n < lower) genPasswords

genPasswords :: [String]
genPasswords = gen 6 Nothing False 0

-- Generate all valid passwords of given length.
-- If `hasRepeat` then the requirement of 2 repeated adjacent digits has been met.
gen :: Int -> Maybe Int -> Bool -> Int -> [String]
gen n prevDigit hasRepeat streakLength =
  if n == 0 then
    if hasRepeat || streakLength == 2 then [""] else []
  else
    let startDigit = fromMaybe 0 prevDigit
        digitRange = [startDigit..9]
    in flip concatMap digitRange $ \digit1 ->
        let matchesLast = prevDigit == Just digit1
            hasRepeatNow = hasRepeat || streakLength == 2 && not matchesLast
            streakLengthNow = if matchesLast then streakLength + 1 else 1
        in map (show digit1 ++) (gen (n - 1) (Just digit1) hasRepeatNow streakLengthNow)
