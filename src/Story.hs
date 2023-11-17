module Story
  ( tellStory,
    waitForStorySolution,
    getStory,
    Story (..),
  )
where

import Cipher (caeserCipher)
import KeyEvents (disableInputEcho, enableInputEcho)
import StringReplace (replaceSubstring)
import Typer (putTextNl)

data Story = Story
  { storyText :: [String],
    storyHint :: String,
    storySecret :: String,
    storyCypherFunction :: Maybe (String -> String),
    nextRoomName :: Maybe String
  }

-- | List of all stories
stories :: [Story]
stories =
  [ Story
      { storyText =
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
      { storyText =
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
      { storyText =
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
      { storyText =
          [ ""
          ],
        storyHint = "",
        storySecret = "",
        storyCypherFunction = Nothing,
        nextRoomName = Nothing
      }
  ]

-- | Returns the story with the given number from the stories list, if it exists
getStory :: Int -> Story
getStory a = if a <= length stories then stories !! (a - 1) else error "Story does not exist"

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
printStory Story {storyText = text} = mapM_ putTextNl text

-- | Prints the story to the console. Replaces the "SECRET" and "HINT" placeholders with the actual secret and hint text.
tellStory :: Story -> IO ()
tellStory story = do
  disableInputEcho
  printStory $ replaceStoryHint $ replaceStorySecret story
  enableInputEcho

-- | Waits for the user to enter the correct solution for the given story.
waitForStorySolution :: Story -> IO ()
waitForStorySolution story = do
  input <- getLine
  if isStoryInputCorrect story input
    then do
      return ()
    else do
      putStrLn "Das Terminal piept einmal kurz und leuchtet rot auf."
      waitForStorySolution story

-- | Checks if the given input is correct for the given story.
isStoryInputCorrect :: Story -> String -> Bool
isStoryInputCorrect story input = (storySecret story == input) || (storySecret story == "")