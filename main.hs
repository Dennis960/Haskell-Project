import Data.Char

main = do
    putStrLn "What text would you like to encrypt?"
    text <- getLine
    putStrLn "How many letters should we shift?"
    shift <- getLine
    let shiftNum = read shift :: Int
    putStrLn $ caeserCipher text shiftNum

-- Caeser Cipher
caeserCipher :: String -> Int -> String
caeserCipher [] _ = []
caeserCipher (x:xs) n = (shiftChar x n) : (caeserCipher xs n)

shiftChar :: Char -> Int -> Char
shiftChar c n = chr $ ord c + n

-- possible encryption algorithms:
-- Caesar Cipher
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