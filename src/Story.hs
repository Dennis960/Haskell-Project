module Story
  ( getGameLoopElement,
  )
where

import Cipher (caeserCipher)
import GameLoopElement
  ( GameLoopElement (..),
    RoomElement (..),
    StorySecret
      ( StorySecret,
        storyCypherFunction,
        storySecret,
        storySecretText
      ),
    StoryText (StoryText, storyTextText),
  )

-- | List of all gameLoopElements
gameLoopElements :: [GameLoopElement]
gameLoopElements =
  [ StoryTextItem
      StoryText
        { storyTextText =
            [ "«Ich bin so kurz vor meinem Ziel.",
              "Ich spüre die utlimative Weltformel schon in meinen Händen.",
              "Mein ganzes Leben hat mich auf diesen Moment vorbereitet.",
              "Nun stehe ich vor einer verschlossenen Tür.",
              "Es scheint ein uralter Mechanismus zu sein. Leuchtende Schrift ist in den Griff eingraviert.",
              "Ich weiß, was sie bedeutet. Der große Meister hatte schon immer eine Vorliebe für Verschlüsselungen.",
              "Ich muss nur jeden Buchstaben des Schlüssels um drei Stellen im Alphabet verschieben, dann öffnet sich die Tür.»",
              "",
              "Du legst das Tagebuch beiseite und gehst zur Tür."
            ]
        },
    WaitForEnterKeyItem,
    RoomItem
      RoomElement
        { roomName = "passage"
        },
    StorySecretItem
      StorySecret
        { storySecretText =
            [ "Am Türgriff liest du folgenden Text:",
              "$SECRET",
              "Caesar Shift 3",
              "Daneben ist ein Terminal, in welches du einen Text eingeben kannst."
            ],
          storySecret = "Willkommen im Spiel",
          storyCypherFunction = (`caeserCipher` 3)
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "Mit einem lauten Krachen öffnet sich die alte Tür. Du gehst hindurch und merkst, wie die Luft deutlich kühler wird. Die Tür hinter dir schließt sich.",
              "Zum Glück hast du das Tagebuch nicht liegen gelassen.",
              "",
              "«Es ist erstaunlich, welch riesige Mechanismen der große Meister erschaffen hat, um seine wichtigsten Schätze zu schützen.",
              "Ich habe nur Gerüchte gehört, aber es heißt, dass das gesamte Gebäude sofort einstürtzen würde, wenn man einen Raum betritt, ohne das vorherige Rätsel gelöst zu haben.",
              "Damit wäre die Weltformel für immer verloren.»"
            ]
        },
    WaitForEnterKeyItem,
    RoomItem
      RoomElement
        { roomName = "passage_corner"
        }
  ]

-- | Returns the gameLoopElement with the given number from the gameLoopElements list, if it exists
getGameLoopElement :: Int -> GameLoopElement
getGameLoopElement a = if a <= length gameLoopElements then gameLoopElements !! (a - 1) else error "GameLoopElement does not exist"
