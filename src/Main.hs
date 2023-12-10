import GameLoopElement
  ( GameLoopElement (RoomItem, StorySecretItem, StoryTextItem, WaitForEnterKeyItem),
    RoomElement (roomName),
    tellStory,
    waitForStorySolution,
  )
import KeyEvents (waitForEnterKey)
import OptionMenu (selectOption)
import Room (loadRoom, loopPlayerInsideRoom, printRoom)
import Story (gameLoopElementsWithType, gameLoopElementsWithSolution, getGameLoopElement, lengthOfGameLoopElements)

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

  if gameElementNumber == lengthOfGameLoopElements
    then do
      putStrLn "Du hast das Spiel erfolgreich beendet!"
    else do
      run (GameState {gameElementNumber = gameElementNumber + 1})

data PlayMode = PlayMode | CheatMode deriving (Show, Eq)

main :: IO ()
main = do
  playMode <- selectOption "Wähle einen Spielmodus" [(PlayMode, "Normal"), (CheatMode, "Cheat")]
  
  startIndex <-
    if playMode == CheatMode
      then selectOption "(Cheat Modus aktiviert) Wähle einen Startpunkt" (gameLoopElementsWithType ++ [(-1, "Alle Lösungen anzeigen")])
      else return 1
  if startIndex == -1 then do
    putStrLn "Lösungen:"
    mapM_ print gameLoopElementsWithSolution
    putStrLn "(Enter drücken zum Fortfahren)"
    waitForEnterKey
    main
  else do
    return ()
  run (GameState {gameElementNumber = startIndex})