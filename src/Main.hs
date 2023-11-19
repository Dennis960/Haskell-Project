import GameLoopElement
  ( GameLoopElement (RoomItem, StorySecretItem, StoryTextItem),
    RoomElement (roomName),
    tellStory,
    waitForStorySolution,
  )
import KeyEvents (Direction (..), waitForEnterKey)
import Room (Room, loadRoom, loopPlayerInsideRoom, printRoom)
import Story (getGameLoopElement)

newtype GameState = GameState
  { gameElementNumber :: Int
  }
  deriving (Show)

run :: GameState -> IO ()
run gameState@(GameState gameElementNumber) = do
  let gameLoopElement = getGameLoopElement gameElementNumber
  case gameLoopElement of
    StorySecretItem storySecret -> do
      tellStory (StorySecretItem storySecret)
      waitForStorySolution (StorySecretItem storySecret)
    StoryTextItem storyText -> do
      tellStory (StoryTextItem storyText)
      waitForEnterKey
    RoomItem roomElement -> do
      room <- loadRoom (roomName roomElement)
      printRoom room
      loopPlayerInsideRoom room

  run (GameState {gameElementNumber = gameElementNumber + 1})

main = run (GameState 1)