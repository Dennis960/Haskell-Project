module Computer
  ( getInput,
    flashRed,
    flashGreen,
    clearComputerAscii,
  )
where

import Control.Concurrent (threadDelay)
import KeyEvents (disableInputEcho, enableInputEcho, getKey)
import Text.XHtml (green)

computerAscii =
  "             ________________________________________________                    \n\
  \            /                                                \\                  \n\
  \           |    _________________________________________     |                  \n\
  \           |   |                                         |    |                  \n\
  \           |   |  >> ################################    |    |                  \n\
  \           |   |     ################################    |    |                  \n\
  \           |   |     ################################    |    |                  \n\
  \           |   |     ################################    |    |                  \n\
  \           |   |_________________________________________|    |                  \n\
  \           |                                                  |                  \n\
  \            \\_________________________________________________/                 \n\
  \                   \\___________________________________/                        \n\
  \                ___________________________________________                      \n\
  \             _-'    .-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.  --- `-_                   \n\
  \          _-'.-.-. .---.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.  .-.-.`-_                \n\
  \       _-'.-.-.-. .---.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-`__`. .-.-.-.`-_             \n\
  \    _-'.-.-.-.-. .-----.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-----. .-.-.-.-.`-_          \n\
  \ _-'.-.-.-.-.-. .---.-. .-------------------------. .-.---. .---.-.-.-.`-_       \n\
  \:-------------------------------------------------------------------------:      \n\
  \`---._.-------------------------------------------------------------._.---'      "

-- | Removes the entire computer ascii from the console
clearComputerAscii :: IO ()
clearComputerAscii = putStrLn $ "\ESC[" ++ show (length (lines computerAscii) + 1) ++ "A"

replaceInString :: String -> Char -> String
replaceInString [] _ = []
replaceInString (x : xs) char = if x == '#' then char : xs else x : replaceInString xs char

countNumberOfCharsInString :: String -> Char -> Int
countNumberOfCharsInString [] _ = 0
countNumberOfCharsInString (x : xs) char = if x == char then 1 + countNumberOfCharsInString xs char else countNumberOfCharsInString xs char

replaceMultipleInString :: String -> String -> String
replaceMultipleInString = foldl replaceInString

replaceMultipleInStringAndFillWithSpaces :: String -> String -> String
replaceMultipleInStringAndFillWithSpaces original replace = replaceMultipleInString original replaceFillCropped
  where
    numberOfCharsToReplace = countNumberOfCharsInString original '#'
    replaceFill = replace ++ replicate (numberOfCharsToReplace - length replace) ' '
    replaceFillCropped = take numberOfCharsToReplace replaceFill

printComputerAsciiWithString :: String -> IO ()
printComputerAsciiWithString string = putStrLn $ replaceMultipleInStringAndFillWithSpaces computerAscii string

-- | Gets a string from the user, the string that the user types is displayed on the computer screen
getInput :: IO String
getInput = do
  disableInputEcho
  printComputerAsciiWithString ""
  input <- getInput' ""
  enableInputEcho
  return input
  where
    getInput' :: String -> IO String
    getInput' str = do
      clearComputerAscii
      printComputerAsciiWithString (str ++ "█")
      key <- getKey
      case key of
        "\DEL" -> if not (null str) then getInput' (init str) else getInput' ""
        "\n" -> return str
        _ -> if length key == 1 then getInput' (str ++ key) else getInput' str

redColorCode :: Int
redColorCode = 31

greenColorCode :: Int
greenColorCode = 32

-- | Flashes the computer led with the given color for the given number of times with the given delay in milliseconds
flashColor :: Int -> Int -> Int -> IO ()
flashColor colorCode numberOfTimes delay
  | numberOfTimes == 0 = return ()
  | otherwise = do
      disableInputEcho
      let pos = 1081
      clearComputerAscii
      if even numberOfTimes
        then putStrLn $ take (pos - 1) computerAscii ++ "\ESC[" ++ show colorCode ++ "m●\ESC[0m" ++ drop pos computerAscii
        else putStrLn computerAscii
      threadDelay (delay * 1000)
      flashColor colorCode (numberOfTimes - 1) delay
      enableInputEcho

-- | Flashes the computer led red for the given number of times with the given delay in milliseconds
flashRed :: Int -> Int -> IO ()
flashRed = flashColor redColorCode

-- | Flashes the computer led green for the given number of times with the given delay in milliseconds
flashGreen :: Int -> Int -> IO ()
flashGreen = flashColor greenColorCode