import GameLoopElement
  ( GameLoopElement (RoomItem, StorySecretItem, StoryTextItem, WaitForEnterKeyItem),
    RoomElement (roomName),
    tellStory,
    waitForStorySolution,
  )
import KeyEvents (waitForEnterKey)
import OptionMenu (selectOption)
import Room (loadRoom, loopPlayerInsideRoom, printRoom)
import Story (gameLoopElementsWithType, getGameLoopElement)

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
    RoomItem roomElement -> do
      room <- loadRoom (roomName roomElement)
      printRoom room
      loopPlayerInsideRoom room
    WaitForEnterKeyItem -> do
      putStrLn "(Enter drücken zum Fortfahren)"
      waitForEnterKey

  run (GameState {gameElementNumber = gameElementNumber + 1})

main = do
  startIndex <- selectOption "Wähle ein GameLoopElement" gameLoopElementsWithType
  run (GameState {gameElementNumber = startIndex})