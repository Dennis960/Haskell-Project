module Cipher where

import Data.Char (chr, isLower, isUpper, ord, toLower, isLetter)

-- Reverse Text ---------
reverseText:: String -> String
reverseText [] = []
reverseText [x] = [x]
reverseText (x:xs) = reverseText xs ++ [x]


-- Caeser Cipher -------
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


-- Vigenere Cipher ---------
vigenereCipher:: String -> String -> String
vigenereCipher [] _ = []
vigenereCipher word key =  cipherVigenere word (prepForCipher key) 0
-- | when called Int has to be 0 to function correctly as the index of the key
  where
  cipherVigenere:: String -> String -> Int -> String
  cipherVigenere [] _ _ = []
  cipherVigenere (x : xs) key keyIndex = shiftByKey x (key !! keyIndex) : cipherVigenere xs key ((keyIndex + 1) `mod` length key)
    where
      shiftLower :: Char -> Char -> Char
      shiftLower c key = chr $ ((ord c - ord 'a') + (ord key - ord 'a')) `mod` 26 + ord 'a'
      shiftUpper :: Char -> Char -> Char
      shiftUpper c key = chr $ ((ord c - ord 'A') + (ord key - ord 'a')) `mod` 26 + ord 'A'
      shiftByKey :: Char -> Char -> Char
      shiftByKey c key
       | isLower x = shiftLower x key
       | isUpper x = shiftUpper x key
       | otherwise = x


-- | removes non-alphabetical symbols (and whitespaces) and makes everything lowercase
prepForCipher :: String -> String
prepForCipher [] = []
prepForCipher word = toLowerCase (removeNonAlphabetical word)
  where

    toLowerCase:: String -> String
    toLowerCase = map toLower

    removeNonAlphabetical :: String -> String
    removeNonAlphabetical [] = []
    removeNonAlphabetical (x : xs)
      | isLetter x = x : removeNonAlphabetical xs
      | otherwise = removeNonAlphabetical xs

--Morse Code ------

-- | never to be touched D:<
morseTable :: [(Char, String)]
morseTable = [('a', ".-"), ('b', "-..."), ('c', "-.-."), ('d', "-.."),
              ('e', "."), ('f', "..-."), ('g', "--."), ('h', "...."),
              ('i', ".."), ('j', ".---"), ('k', "-.-"), ('l', ".-.."),
              ('m', "--"), ('n', "-."), ('o', "---"), ('p', ".--."),
              ('q', "--.-"), ('r', ".-."), ('s', "..."), ('t', "-"),
              ('u', "..-"), ('v', "...-"), ('w', ".--"), ('x', "-..-"),
              ('y', "-.--"), ('z', "--..")]

morseCode:: String -> String
morseCode [] = []
morseCode (x : xs)
  | toLower x `elem` ['a'..'z'] = getFromTable (toLower x) morseTable ++ " " ++ morseCode xs
  -- word separators are three spaces
  | x == ' ' = "   " ++  morseCode xs
  | otherwise = x : morseCode xs

--Tap Code -------
-- table that assigns a letter to tap pattern; table-entry-pattern: (letter, (x-Coord, y-Coord))
-- 'c' and 'k' are the same, so 'k' is skipped in the table
tapTable :: [(Char, (Integer, Integer))]
tapTable = zip ['a'..'j'] [(x, y) | x <- [1..2], y <- [1..5]]
           ++ zip ['l'..'z'] [(x, y) | x <- [3..5], y <- [1..5]]


tapCode:: String -> String
tapCode [] = []
--tapCode [x] = "hi" --TODO do the same as below just no space at the end
tapCode (x : xs)
  | lower == 'k' = tapCode ('c' : xs)
  | lower `elem` ['a'..'z'] = let coords = getFromTable lower tapTable in
    printTaps (fst coords) ++ " " ++ printTaps (snd coords) ++ (if null xs then "" else " ") ++ tapCode xs
  | otherwise = x : tapCode xs
    where
      lower = toLower x

      printTaps :: Integer -> String
      printTaps 0 = []
      printTaps x = "tap"++ printTaps (x - 1)

-- | get ciphered symbol with corresponding letter
getFromTable:: Eq a => a -> [(a,b)] -> b
getFromTable c ((letter, wanted) : xs) = if letter == c then wanted else getFromTable c xs


-- Substitution Cipher
-- Atbash Cipher
-- Rail Fence Cipher
-- Binary Encoding
-- Base64 Encoding
-- Pig Latin
-- Playfair Cipher
-- Transposition Cipher
-- Binary Coded Decimal (BCD)
-- Semaphore Encoding
-- Tap Code
-- Semaphore Flag Code
-- Trifid Cipher
-- Caesar Box Cipher
-- Tap Code (Modified)