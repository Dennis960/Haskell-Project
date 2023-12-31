module Typer (putTextNl, clearLines, clearScreenMoveHome) where

import Control.Concurrent (threadDelay)
import KeyEvents (getKey, hasKey)
import System.IO (hFlush, stdout)

defaultTypingDelay :: Int
defaultTypingDelay = 50000

-- | Prints the given text to the console, character by character, with a delay of defaultTypingDelay microseconds.
putTextNl :: String -> IO ()
putTextNl [] = putStrLn "\n"
putTextNl (c : text) = do
  putStr [c]
  hFlush stdout -- flush the buffer, used to immediately print the character instead of waiting for a newline
  isKeyPressed <- hasKey
  if isKeyPressed
    then do
      getKey -- discard the key
      putStr text -- print immediately if a key is pressed
      putTextNl ""
    else do
      threadDelay defaultTypingDelay
      putTextNl text

-- | Clears the given number of lines in the console
clearLines :: Int -> IO ()
clearLines n = do
  putStr "\r"
  putStr $ "\ESC[" ++ show n ++ "A" -- move the cursor n lines up
  putStr "\ESC[J" -- clear the screen from the cursor position to the end of the screen

-- | Clear the screen and move the cursor to the top left corner
clearScreenMoveHome :: IO ()
clearScreenMoveHome = putStr "\ESC[2J\ESC[H"