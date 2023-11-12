import KeyEvents (Direction (..), getDirectionKey, getKey)
import Room (Room, isPlayerTouchingStory, loadRoom, printRoom, roomDirectionMovePlayer)
import Story (isStoryInputCorrect, tellStory)
import System.IO (hFlush, stdout)

data GameState = GameState
  { roomNumber :: Int,
    currentRoom :: Room
  }
  deriving (Show)

loop :: GameState -> IO ()
loop gameState = do
  let room = currentRoom gameState
  printRoom room

  if isPlayerTouchingStory room
    then do
      tellStory (roomNumber gameState)
      waitForStorySolution gameState
    else do
      return ()

  direction <- getDirectionKey
  let newRoom = roomDirectionMovePlayer room direction
  loop (GameState (roomNumber gameState) newRoom)

-- TODO extract this function
waitForStorySolution :: GameState -> IO ()
waitForStorySolution gameState = do
  input <- getLine
  if isStoryInputCorrect (roomNumber gameState) input
    then do
      putStrLn "Richtig!"
      loop gameState {roomNumber = roomNumber gameState + 1}
    else do
      putStr "Falsch! Versuche es gerne noch einmal.\n>>"
      hFlush stdout
      waitForStorySolution gameState

main = do
  room <- loadRoom 1
  loop (GameState 1 room)