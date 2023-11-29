module KeyEvents
  ( KeyCode (..),
    getKeyCode,
    getKeyCodeNonBlocking,
    hasPressedEnter,
    enableInputEcho,
    disableInputEcho,
    waitForEnterKey
  )
where

import Control.Monad
import System.IO
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Char
import Foreign.C.Types

data KeyCode = KeyCodeUp | KeyCodeDown | KeyCodeLeft | KeyCodeRight | KeyCodeEnter | KeyCodeNotAvailable deriving (Show, Eq)

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

-- | Returns a character representing the key that the user has pressed. Blocks until the user presses any key
getKey :: IO Char
getKey = do
  disableInputEcho
  c <- getChar
  enableInputEcho
  clearBuffer
  return c

getKeyCode :: IO KeyCode
getKeyCode = do
  key <- getKey
  return $ keyToKeyCode key

-- | Converts a key to a key code. If the key is not supported, KeyCodeNotAvailable is returned.
keyToKeyCode :: Char -> KeyCode
keyToKeyCode key = case key of
  'w' -> KeyCodeUp
  's' -> KeyCodeDown
  'd' -> KeyCodeRight
  'a' -> KeyCodeLeft
  '\n' -> KeyCodeEnter
  ' ' -> KeyCodeEnter
  '\r' -> KeyCodeEnter
  _ -> KeyCodeNotAvailable

-- | Returns the keycode of the key that the user has pressed. Blocks until the user presses any key.
getKeyBlocking :: IO KeyCode
getKeyBlocking = do
  disableInputEcho
  keycode <- keyToKeyCode <$> getKey
  enableInputEcho
  return keycode

-- | Returns the keycode of the key that the user has pressed. Does not block if the user has not pressed any key.
getKeyCodeNonBlocking :: IO KeyCode
getKeyCodeNonBlocking = do
  hasKey <- hasKey
  if hasKey
    then getKeyBlocking
    else return KeyCodeNotAvailable

-- | Waits for the user to press the enter key.
waitForEnterKey :: IO ()
waitForEnterKey = do
  keycode <- getKeyBlocking
  if keycode == KeyCodeEnter
    then return ()
    else waitForEnterKey

-- | Clears the input buffer for stdin.
clearBuffer :: IO ()
clearBuffer = do
  hasKey <- hReady stdin
  if hasKey
    then do
      hGetChar stdin
      clearBuffer
    else do
      return ()

-- | Returns true if the user has pressed the enter key.
hasPressedEnter :: IO Bool
hasPressedEnter = do
  keycode <- getKeyCodeNonBlocking
  return $ keycode == KeyCodeEnter