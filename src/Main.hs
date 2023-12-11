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
      putStrLn ""
      putStrLn "Ende."
    else do
      run (GameState {gameElementNumber = gameElementNumber + 1})

data PlayMode = PlayMode | CheatMode deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn title
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

title :: String
title = "   _____ _       _                 __  __           _                     \n\
\  / ____(_)     | |               |  \\/  |         | |                    \n\
\ | |     _ _ __ | |__   ___ _ __  | \\  / | __ _  __| |_ __   ___  ___ ___ \n\
\ | |    | | '_ \\| '_ \\ / _ \\ '__| | |\\/| |/ _` |/ _` | '_ \\ / _ \\/ __/ __|\n\
\ | |____| | |_) | | | |  __/ |    | |  | | (_| | (_| | | | |  __/\\__ \\__ \\\n\
\  \\_____|_| .__/|_| |_|\\___|_|    |_|  |_|\\__,_|\\__,_|_| |_|\\___||___/___/\n\
\          | |                                                             \n\
\          |_|                                                             "