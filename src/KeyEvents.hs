module KeyEvents
  ( Direction (..),
    hasKey,
    getKey,
    getDirectionKey,
    enableInputEcho,
    disableInputEcho,
    waitForEnterKey,
  )
where

import Control.Monad (unless)
import System.IO (BufferMode (LineBuffering, NoBuffering), hReady, hSetBuffering, hSetEcho, stdin)

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight | DirectionNone deriving (Show)

type Key = [Char]

-- | Enables input echo, which means that the user can see what they are typing and the input is buffered allowing the user to use backspace.
enableInputEcho :: IO ()
enableInputEcho = do
  hSetEcho stdin True
  hSetBuffering stdin LineBuffering

-- | Disables input echo, which means that the user cannot see what they are typing and the input is not buffered. The user cannot use backspace.
disableInputEcho :: IO ()
disableInputEcho = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

hasKey :: IO Bool
hasKey = hReady stdin

-- | Returns a list of characters representing the key that the user has pressed. Blocks until the user presses any key
getKey :: IO Key
getKey = do
  char <- getChar
  more <- hReady stdin
  if more
    then (char :) <$> getKey
    else do
      return [char]

-- | Converts a key to a direction. If the key is not an arrow key, DirectionNone is returned.
keyToDirection :: Key -> Direction
keyToDirection key = case key of
  "\ESC[A" -> DirectionUp
  "\ESC[B" -> DirectionDown
  "\ESC[C" -> DirectionRight
  "\ESC[D" -> DirectionLeft
  _ -> DirectionNone

-- | Returns the direction key that the user has pressed. Blocks until the user presses any key.
getDirectionKey :: IO Direction
getDirectionKey = do
  disableInputEcho
  direction <- keyToDirection <$> getKey
  enableInputEcho
  return direction

waitForEnterKey :: IO ()
waitForEnterKey = do
  disableInputEcho
  key <- getKey
  enableInputEcho
  unless (key == "\n") waitForEnterKey