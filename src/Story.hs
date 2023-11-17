module Story
  ( tellStory,
    isStoryInputCorrect,
    getStory,
    Story (..),
  )
where

import Cipher (caeserCipher)
import Control.Concurrent (threadDelay)
import Control.Monad.RWS (gets)
import KeyEvents (disableInputEcho, enableInputEcho, getKey, hasKey)
import StringReplace (replaceSubstring)
import System.IO (hFlush, stdout)

defaultTypingDelay :: Int
defaultTypingDelay = 50000

data Story = Story
  { storyNumber :: Int,
    storyText :: [String],
    storyHint :: String,
    storySecret :: String,
    storyCypherFunction :: Maybe (String -> String),
    nextRoomName :: Maybe String
  }

-- | List of all stories
stories :: [Story]
stories =
  [ Story
      { storyNumber = 1,
        storyText =
          [ "«Ich bin so kurz vor meinem Ziel.",
            "Ich spüre die utlimative Weltformel schon in meinen Händen.",
            "Mein ganzes Leben hat mich auf diesen Moment vorbereitet.",
            "Nun stehe ich vor einer verschlossenen Tür.",
            "Es scheint ein uralter Mechanismus zu sein. Leuchtende Schrift ist in den Griff eingraviert.",
            "Ich weiß, was sie bedeutet. Der große Meister hatte schon immer eine Vorliebe für Verschlüsselungen.",
            "Ich muss nur jeden Buchstaben des Schlüssels um drei Stellen im Alphabet verschieben, dann öffnet sich die Tür.»",
            "",
            "Du legst das Tagebuch beiseite und gehst zur Tür.",
            "(Enter drücken zum Fortfahren)"
          ],
        storyHint = "",
        storySecret = "",
        storyCypherFunction = Nothing,
        nextRoomName = Just "passage"
      },
    Story
      { storyNumber = 2,
        storyText =
          [ "Am Türgriff liest du folgenden Text:",
            "$SECRET",
            "$HINT",
            "Daneben ist ein Terminal, in welches du einen Text eingeben kannst.",
            ">>"
          ],
        storyHint = "Caesar Shift 3",
        storySecret = "Willkommen im Spiel",
        storyCypherFunction = Just (`caeserCipher` 3),
        nextRoomName = Nothing
      },
    Story
      { storyNumber = 3,
        storyText =
          [ "Mit einem lauten Krachen öffnet sich die alte Tür. Du gehst hindurch und merkst, wie die Luft deutlich kühler wird. Die Tür hinter dir schließt sich.",
            "Zum Glück hast du das Tagebuch nicht liegen gelassen.",
            "",
            "«Es ist erstaunlich, welch riesige Mechanismen der große Meister erschaffen hat, um seine wichtigsten Schätze zu schützen.",
            "Ich habe nur Gerüchte gehört, aber es heißt, dass das gesamte Gebäude sofort einstürtzen würde, wenn man einen Raum betritt, ohne das vorherige Rätsel gelöst zu haben.",
            "Damit wäre die Weltformel für immer verloren.»"
          ],
        storyHint = "",
        storySecret = "",
        storyCypherFunction = Nothing,
        nextRoomName = Just "passage_corner"
      },
    Story
      { storyNumber = 4,
        storyText =
          [ ""
          ],
        storyHint = "",
        storySecret = "",
        storyCypherFunction = Nothing,
        nextRoomName = Nothing
      }
  ]

-- | Prints the given text to the console, character by character, with a delay of defaultTypingDelay microseconds.
putTextNl :: String -> IO ()
putTextNl [] = putStrLn "\n"
putTextNl (c : text) = do
  putStr [c]
  hFlush stdout -- flush the buffer, used to immediately print the character instead of waiting for a newline
  isKeyPressed <- hasKey
  if isKeyPressed
    then do
      getKey -- discard the key
      putStr text -- print immediately if a key is pressed
      putTextNl ""
    else do
      threadDelay defaultTypingDelay
      putTextNl text

-- | Returns the story with the given number from the stories list, if it exists
getStory :: Int -> Story
getStory a
  | null filterStoriesWithNumber = error "Story not found"
  | otherwise = head filterStoriesWithNumber
  where
    filterStoriesWithNumber = filter (\s -> storyNumber s == a) stories

-- | Replaces the "SECRET" placeholder in the story with the actual secret text
replaceStorySecret :: Story -> Story
replaceStorySecret story@Story {storyCypherFunction = Nothing} = story
replaceStorySecret story@Story {storyCypherFunction = Just cypherFunction, storySecret = secret} = story {storyText = replaceSecret (storyText story)}
  where
    replaceSecret :: [String] -> [String]
    replaceSecret [] = []
    replaceSecret (x : xs) = replaceSubstring x "$SECRET" (cypherFunction secret) : replaceSecret xs

-- | Replaces the "HINT" placeholder in the story with the actual hint text
replaceStoryHint :: Story -> Story
replaceStoryHint story = story {storyText = replaceHint (storyText story)}
  where
    replaceHint :: [String] -> [String]
    replaceHint [] = []
    replaceHint (x : xs) = replaceSubstring x "$HINT" (storyHint story) : replaceHint xs

-- | Prints the story text to the console.
printStory :: Story -> IO ()
printStory Story {storyText = text} = do
  -- for every element of text, print it with putTextNl
  mapM_ putTextNl text

-- | Prints the story with the given number to the console. Replaces the "SECRET" and "HINT" placeholders with the actual secret and hint text.
tellStory :: Story -> IO ()
tellStory story = do
  disableInputEcho
  printStory $ replaceStoryHint $ replaceStorySecret story
  enableInputEcho
  hFlush stdout

-- | Checks if the given input is correct for the given story number.
isStoryInputCorrect :: Story -> String -> Bool
isStoryInputCorrect story input = (storySecret story == input) || (storySecret story == "")