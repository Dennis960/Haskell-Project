module Cipher where

import Data.Char (chr, isLower, isUpper, ord)

-- Caeser Cipher
caeserCipher :: String -> Int -> String
caeserCipher [] _ = []
caeserCipher (x : xs) n = shiftChar x n : caeserCipher xs n
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


--TODO doesn't check all possible keys yet, no loop, no whitespace
vigenereCipher :: String -> String -> String
vigenereCipher [] _ = []
vigenereCipher (x : xs) (k : restKey) = shiftChar x k : vigenereCipher xs restKey 
  where
    shiftLower :: Char -> Char -> Char
    shiftLower c key = chr $ ((ord c - ord 'a') + (ord key - ord 'a')) `mod` 26 + ord 'a'
    shiftUpper :: Char -> Char -> Char
    shiftUpper c key = chr $ (ord c - ord 'A' + (ord key - ord 'A')) `mod` 26 + ord 'A'
    shiftChar :: Char -> Char -> Char
    shiftChar c key
      | isLower c = shiftLower c key
      | isUpper c = shiftUpper c key
      | otherwise = c

-- Reverse Text
reverseText:: String -> String
reverseText [] = []
reverseText [x] = [x]
reverseText (x:xs) = reverseText xs ++ [x]

-- Vigenère Cipher
-- Morse Code
-- Substitution Cipher
-- Atbash Cipher
-- Rail Fence Cipher
-- Binary Encoding
-- Base64 Encoding
-- Pig Latin
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