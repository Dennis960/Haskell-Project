module Story
  ( tellStory,
    isStoryInputCorrect,
  )
where

import Cipher (caeserCipher)
import Control.Concurrent (threadDelay)
import StringReplace (replaceSubstring)
import System.IO (hFlush, stdout)

defaultTypingDelay :: Int
defaultTypingDelay = 50000

data Story = Story
  { storyNumber :: Int,
    storyText :: [String],
    storyHint :: String,
    storySecret :: String,
    storyCypherFunction :: String -> String
  }

-- | List of all stories
stories :: [Story]
stories =
  [ Story
      { storyNumber = 532,
        storyText =
          [ "Du wachst auf. Du weißt nicht, wo du bist. Alles ist dunkel.",
            "An deinem Handgelenk klebt ein grüner Zettel. Du liest die Worte: »Wenn du dich weigerst, zu kooperieren, dann wirst du es nicht mehr hinausschaffen.«",
            "Vor lauter Schock fällt dir das Handy aus der Hand. Es geht kaputt. Du bist allein. Niemand kann dir helfen. Du musst dich selbst retten.",
            "Nach einer Weile fällt dir leuchtende Schrift auf, doch du weißt nicht, was sie bedeutet.",
            "»SECRET«",
            "Weiter unten steht ein Hinweis: »HINT«",
            "Plötzlich leuchtet ein Terminal auf. Du kannst einen Text eingeben. Neben dem Terminal steht eine Zahl: 532."
          ],
        storyHint = "Schlüssel: 3. Caesar Shift. Viel Erfolg",
        storySecret = "Willkommen im Spiel",
        storyCypherFunction = (`caeserCipher` 3)
      }
  ]

-- | Prints the given text to the console, character by character, with a delay of defaultTypingDelay microseconds.
putTextNl :: String -> IO ()
putTextNl [] = putStrLn "\n"
putTextNl (c : text) = do
  putStr [c]
  hFlush stdout -- flush the buffer, used to immediately print the character instead of waiting for a newline
  threadDelay defaultTypingDelay
  putTextNl text

-- | Returns the story with the given number from the stories list, if it exists
getStory :: Int -> Maybe Story
getStory a
  | null filterStoriesWithNumber = Nothing
  | otherwise = Just $ head filterStoriesWithNumber
  where
    filterStoriesWithNumber = filter (\s -> storyNumber s == a) stories

-- | Replaces the "SECRET" placeholder in the story with the actual secret text
replaceStorySecret :: Story -> Story
replaceStorySecret story@Story {storyCypherFunction = cypherFunction, storySecret = secret} = story {storyText = replaceSecret (storyText story)}
  where
    replaceSecret :: [String] -> [String]
    replaceSecret [] = []
    replaceSecret (x : xs) = replaceSubstring x "SECRET" (cypherFunction secret) : replaceSecret xs

-- | Replaces the "HINT" placeholder in the story with the actual hint text
replaceStoryHint :: Story -> Story
replaceStoryHint story = story {storyText = replaceHint (storyText story)}
  where
    replaceHint :: [String] -> [String]
    replaceHint [] = []
    replaceHint (x : xs) = replaceSubstring x "HINT" (storyHint story) : replaceHint xs

-- | Prints the story text to the console.
printStory :: Story -> IO ()
printStory Story {storyText = text} = do
  -- for every element of text, print it with putTextNl
  mapM_ putTextNl text

-- | Prints the story with the given number to the console. Replaces the "SECRET" and "HINT" placeholders with the actual secret and hint text.
tellStory :: Int -> IO ()
tellStory a = do
  case story of
    Nothing -> putStrLn "Es gibt keine Story für diese Zahl."
    Just story -> do
      printStory $ replaceStoryHint $ replaceStorySecret story
      putStr $ ">> " ++ show (storyNumber story) ++ ": "
      hFlush stdout
  where
    story = getStory a

-- | Checks if the given input is correct for the given story number.
isStoryInputCorrect :: Int -> String -> Bool
isStoryInputCorrect storyNumber input = case story of
  Nothing -> False
  Just story -> storySecret story == input
  where
    story = getStory storyNumber