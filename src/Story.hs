module Story
  ( getGameLoopElement,
    gameLoopElementsWithType,
    gameLoopElementsWithSolution,
  )
where

import Cipher (caeserCipher, reverseText)
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
        { roomName = "room_passage"
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
          storyCypherFunction = reverseText
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
        { roomName = "room_passage_corner"
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "Als du um die Ecke biegst, merkst du nicht, wie du auf eine Falltür trittst.",
              "Du fällst in die Tiefe, doch du landest weich."
            ]
        },
    WaitForEnterKeyItem,
    RoomItem
      RoomElement
        { roomName = "room_cave"
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "Hier scheint irgendeine Art von Tür zu sein.",
              "Es ist gerade so hell genug, dass du im Tagebuch lesen kannst.",
              "Du fragst dich allerdings, wo das Licht herkommt, denn du siehst keine Fenster in der Nähe.",
              "",
              "«Ich habe es geschafft. Ich habe die Weltformel gefunden.",
              "Ich rieche sie, ich schmecke sie.",
              "Die anderen haben mich für verrückt erklärt, aber ich bin mir sicher, dass dieser Fund sie vom Gegenteil überzeugen wird.»",
              "",
              "Du blätterst weiter.",
              "",
              "«Nein! Das darf nicht wahr sein!",
              "Wie konnte ich nur so dumm sein?",
              "Ich hätte den Raum auf Falltüren untersuchen sollen.»",
              "",
              "Du ärgerst dich, dass du dich an diese Stelle nicht erinnert hast."
            ]
        }
  ]

-- | Returns the gameLoopElement with the given number from the gameLoopElements list, if it exists
getGameLoopElement :: Int -> GameLoopElement
getGameLoopElement a = if a <= length gameLoopElements then gameLoopElements !! (a - 1) else error "GameLoopElement does not exist"

gameLoopElementsWithType :: [(Int, String)]
gameLoopElementsWithType = zip [1 ..] (map getGameLoopElementType gameLoopElements)
  where
    getGameLoopElementType :: GameLoopElement -> String
    getGameLoopElementType (StoryTextItem _) = "Story"
    getGameLoopElementType (StorySecretItem _) = "Secret"
    getGameLoopElementType (RoomItem RoomElement {roomName}) = "Room (" ++ roomName ++ ")"
    getGameLoopElementType WaitForEnterKeyItem = "WaitForEnterKey"

gameLoopElementsWithSolution :: [(String, String)]
gameLoopElementsWithSolution = filter (\(_, solution) -> solution /= "") (zip (map getGameLoopElementSecret gameLoopElements) (map getGameLoopElementSolution gameLoopElements))
  where
    getGameLoopElementSolution :: GameLoopElement -> String
    getGameLoopElementSolution (StorySecretItem StorySecret {storySecret}) = storySecret
    getGameLoopElementSolution _ = ""
    getGameLoopElementSecret :: GameLoopElement -> String
    getGameLoopElementSecret (StorySecretItem StorySecret {storySecret, storyCypherFunction}) = storyCypherFunction storySecret
    getGameLoopElementSecret _ = ""