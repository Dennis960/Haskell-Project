module Typer (putTextNl, clearLines) where

import Control.Concurrent (threadDelay)
import KeyEvents (getKeyCodeNonBlocking, KeyCode (KeyCodeEnter))
import System.IO (hFlush, stdout)

defaultTypingDelay :: Int
defaultTypingDelay = 50000

-- | Prints the given text to the console, character by character, with a delay of defaultTypingDelay microseconds.
putTextNl :: String -> IO ()
putTextNl [] = putStrLn "\n"
putTextNl (c : text) = do
  putStr [c]
  -- hFlush stdout -- flush the buffer, used to immediately print the character instead of waiting for a newline
  threadDelay 1
  putTextNl text
  -- keycode <- getKeyCodeNonBlocking
  -- if keycode == KeyCodeEnter
  --   then do
  --     putStr text -- print immediately if enter key is pressed
  --     putTextNl ""
  --   else do

-- | Clears the given number of lines in the console
clearLines :: Int -> IO ()
clearLines n = do
  putStr "\r"
  putStr $ "\ESC[" ++ show n ++ "A" -- move the cursor n lines up
  putStr "\ESC[J" -- clear the screen from the cursor position to the end of the screen