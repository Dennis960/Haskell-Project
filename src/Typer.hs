module Typer where

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