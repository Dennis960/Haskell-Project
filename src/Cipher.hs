module Cipher where

import Data.Char

-- Caeser Cipher
caeserCipher :: String -> Int -> String
caeserCipher [] _ = []
caeserCipher (x : xs) n = (shiftChar x n) : (caeserCipher xs n)
  where
    shiftLower :: Char -> Int -> Char
    shiftLower c n = chr $ (ord c - ord 'a' + n) `mod` 26 + ord 'a'
    shiftUpper :: Char -> Int -> Char
    shiftUpper c n = chr $ (ord c - ord 'A' + n) `mod` 26 + ord 'A'
    shiftChar :: Char -> Int -> Char
    shiftChar c n
      | isLower c = shiftLower c n
      | isUpper c = shiftUpper c n
      | otherwise = c

-- Vigenère Cipher
-- Morse Code
-- Substitution Cipher
-- Atbash Cipher
-- Rail Fence Cipher
-- Binary Encoding
-- Base64 Encoding
-- Reverse Text
-- Pig Latin
-- Reverse Words
-- Playfair Cipher
-- Scytale Cipher
-- Transposition Cipher
-- Binary Coded Decimal (BCD)
-- Semaphore Encoding
-- Tap Code
-- Semaphore Flag Code
-- Trifid Cipher
-- Caesar Box Cipher
-- Tap Code (Modified)