import KeyEvents (Direction (..), getDirectionKey, getKey)
import Room (Room, isPlayerTouchingStory, loadRoom, printRoom, roomDirectionMovePlayer)
import Story (Story (nextRoomName), getStory, waitForStorySolution, tellStory)
import System.IO (hFlush, stdout)

data GameState = GameState
  { storyNumber :: Maybe Int,
    nextStoryNumber :: Int,
    roomName :: Maybe String
  }
  deriving (Show)

loopPlayerInsideRoom :: GameState -> Room -> IO ()
loopPlayerInsideRoom gameState room = do
  direction <- getDirectionKey
  let newRoom = roomDirectionMovePlayer room direction
  printRoom newRoom
  if isPlayerTouchingStory newRoom
    then do
      run (GameState {storyNumber = Just (nextStoryNumber gameState), nextStoryNumber = nextStoryNumber gameState + 1, roomName = Nothing})
    else do
      loopPlayerInsideRoom gameState newRoom

loopStoryTime :: GameState -> Int -> IO ()
loopStoryTime gameState storyNumber = do
  let story = getStory storyNumber
  tellStory story
  waitForStorySolution story
  case nextRoomName story of
    Nothing -> run (GameState {storyNumber = Just (storyNumber + 1), nextStoryNumber = storyNumber + 2, roomName = Nothing})
    Just nextRoomName -> run (GameState {storyNumber = Nothing, nextStoryNumber = nextStoryNumber gameState, roomName = Just nextRoomName})

run :: GameState -> IO ()
run gameState = do
  case gameState of
    GameState {storyNumber = Just storyNumber, roomName = Nothing} -> do
      loopStoryTime gameState storyNumber
    GameState {storyNumber = Nothing, roomName = Just roomName} -> do
      room <- loadRoom roomName
      printRoom room
      loopPlayerInsideRoom gameState room
    _ -> do
      error "Invalid game state"

main = run (GameState {storyNumber = Just 1, nextStoryNumber = 2, roomName = Nothing})