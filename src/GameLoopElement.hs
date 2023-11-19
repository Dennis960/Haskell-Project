module GameLoopElement where

import GHC.IO.Handle (hFlush)
import KeyEvents (disableInputEcho, enableInputEcho)
import StringReplace (replaceSubstring)
import System.IO (stdout)
import Typer (putTextNl)

data StorySecret = StorySecret
  { storySecretText :: [String],
    storySecret :: String,
    storyCypherFunction :: String -> String
  }

newtype StoryText = StoryText
  { storyTextText :: [String]
  }

newtype RoomElement = RoomElement
  { roomName :: String
  }

data GameLoopElement = StoryTextItem StoryText | StorySecretItem StorySecret | RoomItem RoomElement | WaitForEnterKeyItem

-- | Replaces the "SECRET" placeholder in the StorySecret with the actual secret text
replaceStorySecret :: StorySecret -> StorySecret
replaceStorySecret s@(StorySecret {storySecret = storySecret, storyCypherFunction = cypherFunction}) = s {storySecretText = replaceSecret (storySecretText s)}
  where
    replaceSecret :: [String] -> [String]
    replaceSecret [] = []
    replaceSecret (x : xs) = replaceSubstring x "$SECRET" (cypherFunction storySecret) : replaceSecret xs

-- | Prints the story text of the GameLoopElement to the console. Throws error if the GameLoopElement is not a StoryTextItem or StorySecretItem
printStory :: GameLoopElement -> IO ()
printStory (StorySecretItem StorySecret {storySecretText = text}) = mapM_ putTextNl text
printStory (StoryTextItem StoryText {storyTextText = text}) = mapM_ putTextNl text
printStory _ = error "GameLoopElement is not a StoryTextItem or StorySecretItem"

-- | Prints the story to the console. Replaces the "SECRET" placeholder with the actual secret text.
tellStory :: GameLoopElement -> IO ()
tellStory gameLoopElement@(StorySecretItem storySecret) = do
  disableInputEcho
  printStory (StorySecretItem (replaceStorySecret storySecret))
  enableInputEcho
tellStory gameLoopElement@(StoryTextItem _) = do
  disableInputEcho
  printStory gameLoopElement
  enableInputEcho

-- | Waits for the user to enter the correct solution for the given story.
waitForStorySolution :: GameLoopElement -> IO ()
waitForStorySolution story = do
  putStr ">> "
  hFlush stdout
  input <- getLine
  if isStoryInputCorrect story input
    then do
      return ()
    else do
      putStrLn "Das Terminal piept einmal kurz und leuchtet rot auf."
      waitForStorySolution story

-- | Checks if the given input is correct for the given story.
isStoryInputCorrect :: GameLoopElement -> String -> Bool
isStoryInputCorrect (StorySecretItem story) input = (storySecret story == input) || (storySecret story == "")