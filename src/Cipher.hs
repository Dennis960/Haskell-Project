module Cipher where

import Data.Char (chr, isLower, isUpper, ord, toLower, isSpace)

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


initAndExecuteVigenere:: String -> String -> String
initAndExecuteVigenere [] _ = []
initAndExecuteVigenere word key =  vigenereCipher (toLowerCase word) (toLowerCase key) 0

toLowerCase:: String -> String
toLowerCase word = map toLower word

--when called int has to be 0 to act as pointer
vigenereCipher:: String -> String -> Int -> String
vigenereCipher [] _ _ = []
vigenereCipher (x : xs) key pointer = shiftByKey x (key !! pointer) : vigenereCipher xs key ((pointer + 1) `mod` length key)
  where
    shiftByKey :: Char -> Char -> Char
    shiftByKey c key = chr $ ((ord c - ord 'a') + (ord key - ord 'a')) `mod` 26 + ord 'a'



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