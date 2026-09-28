module Crom.Util.Text (
    padLeft,
    padRight,
    showSigDigits,
    PrettyShow (..),
) where

import Numeric (floatToDigits)

-- | Minimal pretty-printing class.
class PrettyShow a where
    prettyShow :: a -> String

padLeft :: a -> Int -> [a] -> [a]
padLeft x n xs = replicate deficit x ++ xs
  where
    deficit = max 0 (n - length xs)

padRight :: a -> Int -> [a] -> [a]
padRight x n xs = xs ++ replicate deficit x
  where
    deficit = max 0 (n - length xs)

showDigit :: Int -> Char
showDigit = toEnum . (+ fromEnum '0')

showDigits :: [Int] -> String
showDigits = fmap showDigit

-- Show number with only n significant digits, in non-scientific notation.
showSigDigits :: Int -> Float -> String
showSigDigits n x = sign ++ whole'' ++ frac''
  where
    (digits, e) = floatToDigits 10 (abs x)
    (whole, frac) = splitAt e (take n digits)
    whole' = padRight 0 (max 1 e) whole -- Always show at least one digit, even if it's just 0
    whole'' = showDigits whole'
    frac' = replicate (negate e) 0 ++ frac
    -- Show decimal part only if it has non-zero elements:
    frac'' =
        if any (/= 0) frac'
            then '.' : showDigits frac'
            else ""
    sign = if x < 0 then "-" else ""
