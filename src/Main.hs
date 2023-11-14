import KeyEvents (Direction (..), getDirectionKey, getKey)
import Room (Room, isPlayerTouchingStory, loadRoom, printRoom, roomDirectionMovePlayer)
import Story (Story (nextRoomNumber), getStory, isStoryInputCorrect, tellStory)
import System.IO (hFlush, stdout)

data GameState = GameState
  { storyNumber :: Maybe Int,
    nextStoryNumber :: Int,
    roomNumber :: Maybe Int
  }
  deriving (Show)

loopPlayerInsideRoom :: GameState -> Room -> IO ()
loopPlayerInsideRoom gameState room = do
  printRoom room
  direction <- getDirectionKey
  let newRoom = roomDirectionMovePlayer room direction
  if isPlayerTouchingStory newRoom
    then do
      run (GameState {storyNumber = Just (nextStoryNumber gameState), nextStoryNumber = nextStoryNumber gameState + 1, roomNumber = Nothing})
    else do
      loopPlayerInsideRoom gameState newRoom

loopStoryTime :: GameState -> Int -> IO ()
loopStoryTime gameState storyNumber = do
  let story = getStory storyNumber
  tellStory story
  waitForStorySolution storyNumber
  case nextRoomNumber story of
    Nothing -> run (GameState {storyNumber = Just (storyNumber + 1), nextStoryNumber = storyNumber + 2, roomNumber = Nothing})
    Just nextRoomNumber -> run (GameState {storyNumber = Nothing, nextStoryNumber = nextStoryNumber gameState, roomNumber = Just nextRoomNumber})

run :: GameState -> IO ()
run gameState = do
  case gameState of
    GameState {storyNumber = Just storyNumber, roomNumber = Nothing} -> do
      loopStoryTime gameState storyNumber
    GameState {storyNumber = Nothing, roomNumber = Just roomNumber} -> do
      room <- loadRoom roomNumber
      loopPlayerInsideRoom gameState room
    _ -> do
      error "Invalid game state"

-- TODO extract this function
waitForStorySolution :: Int -> IO ()
waitForStorySolution storyNumber = do
  input <- getLine
  if isStoryInputCorrect storyNumber input
    then do
      return ()
    else do
      putStr "Das Terminal piept dreimal schnell und leuchtet rot auf.\n>>"
      hFlush stdout
      waitForStorySolution storyNumber

main = run (GameState {storyNumber = Just 1, nextStoryNumber = 2, roomNumber = Nothing})