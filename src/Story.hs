module Story
  ( getGameLoopElement,
    gameLoopElementsWithType,
    gameLoopElementsWithSolution,
    lengthOfGameLoopElements,
  )
where

import Cipher (caesarCipher, morseCode, reverseText, tapCode, columnarTranspositionCipher, vigenereCipher)
import Distribution.Compat.CharParsing (CharParsing (text))
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
              "Ich muss nur den Satz von hinten nach vorne lesen»",
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
              "______________",
              "$SECRET",
              "Reverse Text",
              "______________",
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
              "Ich habe nur Gerüchte gehört, aber es heißt, dass das gesamte Gebäude sofort einstürzen würde, wenn man einen Raum betritt, ohne das vorherige Rätsel gelöst zu haben.",
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
              "Du ärgerst dich, dass du dich an diese Stelle nicht erinnert hast.",
              "Jetzt aufgeben ist jedoch keine Option.",
              "«Vielleicht gibt es ja wieder einen Weg zurück nach oben",
              "Du blickst auf und betrachtest die Tür vor dir.",
              "Die zuvor unbekannte Lichtquelle lässt sich als eine weitere leuchtende Gravierung identifizieren."
            ]
        },
    StorySecretItem
      StorySecret
        { storySecretText =
            [ "______________",
              "$SECRET",
              "Caesar Chiffre, 3",
              "______________",
              "Hektisch blätterst du im Tagebuch weiter bis du die Seite findest:",
              "«Natürlich! Ich muss nur jeden Buchstaben um 3 Stellen zurück verschieben, dann ist das Rätsel gelöst.»",
              "Daneben ist ein Terminal, in welches du einen Text eingeben kannst."
            ],
          storySecret = "Es braucht viel Mut",
          storyCypherFunction = (`caesarCipher` 3)
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "Die Tür schwingt ächzend auf. Du kannst in der Dunkelheit einen weiteren, verwinkelten Flur ausmachen.",
              "«Ich weiß wirklich nicht ob ich den großen Meister bewundern oder verachten soll.",
              "Doch scheinbar ist das der einzige Weg um voranzukommen..»",
              "Die Tür fällt hinter dir zu, nachdem du den neuen Gang betrittst."
            ]
        },
    WaitForEnterKeyItem,
    RoomItem
      RoomElement
        { roomName = "room_cave1"
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "«Endlich, eine Tür! Doch was ist das? Anstatt Buchstaben sind Symbole in die Tür eingeritzt.»",
              "Du fährst mit einem Finger behutsam entlang der Einritzungen in der Tür.",
              "«Hmm, anscheinend hat dieses Rätsel etwas mit Punkten und Strichen zu tun.",
              "Vielleicht kann mir das Tagebuch wieder helfen.»",
              "Nach etwas Herumblättern im Buch findest du auf einer Seite eine kleine Tabelle.",
              "In einer Spalte steht jeweils immer ein Buchstabe, in der anderen Spalte ein Muster aus Punkten und Strichen.",
              "",
              "[ a:  .-  ] | [ b: -... ] | [ c: -.-. ] | [ d:  -.. ]",
              "[ e:   .  ] | [ f: ..-. ] | [ g:  --. ] | [ h: .... ]",
              "[ i:  ..  ] | [ j: .--- ] | [ k:  -.- ] | [ l: .-.. ]",
              "[ m:  --  ] | [ n:  -.  ] | [ o:  --- ] | [ p: .--. ]",
              "[ q: --.- ] | [ r:  .-. ] | [ s:  ... ] | [ t:   -  ]",
              "[ u:  ..- ] | [ v: ...- ] | [ w:  .-- ] | [ x: -..- ]",
              "            | [ y: -.-- ] | [ z: --.. ] |",
              "",
              "Mit dem Tagebuch in der Hand blickst du erneut auf die Einritzungen in der Tür.",
              "Ah und ich darf hier natürlich nicht vergessen, dass der große Meister immer Kleinbuchstaben verwendet hat.»"
            ]
        },
    StorySecretItem
      StorySecret
        { storySecretText =
            [ "______________",
              "$SECRET",
              "Morsecode",
              "______________"
            ],
          storySecret = "immer weiter so",
          storyCypherFunction = morseCode
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "«Geschafft! Die Tür öffnet sich.»",
              "Du gehst hindurch und findest...eine Treppe.",
              "«Eine Treppe? Wie unerwartet...Vielleicht führt sie mich wieder zurück nach oben. Naja alles ist besser als weiter hier unten im Dunkeln zu sitzen.»",
              "Du erklimmst die erste Stufe."
            ]
        },
    WaitForEnterKeyItem,
    RoomItem
      RoomElement
        { roomName = "room_stairs"
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "«Endlich! Ich dachte schon die Stufen würden nie enden.»",
              "Nach Luft schnappend blickst du dich um.",
              "Du findest dich vor einem Tor wieder.",
              "Doch irgendetwas ist anders.", "Du kannst keinen Hinweis zu einem Rätsel finden an dem Tor.",
              "«Das ist doch nicht möglich. Der große Meister hat doch immer Rätsel hinterlassen.»",
              "Ungläubig blätterst du im Tagebuch weiter bis du eine Seite findest, betitelt: 'Klopfcode'.",
              "«Natürlich! Die Klopftöne sind das Rätsel!",
              "Die Anzahl der Töne bestimmt dabei jeweils immer zuerst die Zeile der Übersetzungstabelle und das Klopfen direkt danach entspricht dem der Spalte.»",
              "Ah und ich darf hier natürlich nicht vergessen, dass der große Meister immer Kleinbuchstaben verwendet hat.»",
              "",
              "   | 1 | 2 | 3 | 4 | 5 |",
              "---+---+---+---+---+---+",
              " 1 | a | b |c/k| d | e |",
              "---+---+---+---+---+---+",
              " 2 | f | g | h | i | j |",
              "---+---+---+---+---+---+",
              " 3 | l | m | n | o | p |",
              "---+---+---+---+---+---+",
              " 4 | q | r | s | t | u |",
              "---+---+---+---+---+---+",
              " 5 | v | w | x | y | z |"
            ]
        },
    StorySecretItem
      StorySecret
        { storySecretText =
            [ "______________",
              "$SECRET",
              "Klopfcode",
              "______________"
            ],
          storySecret = "trete ein",
          storyCypherFunction = tapCode
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "«Was würde ich nur ohne das Tagebuch machen. Der große Meister ist wirklich ein Genie.»",
              "Du gehst durch das Tor und findest dich in einem weiteren Raum wieder.",
              "«Ich bin so nah dran. Ich kann es fühlen.»",
              "Du gehst weiter."
            ]
        },
    WaitForEnterKeyItem,
    RoomItem
      RoomElement
        { roomName = "room_upper"
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "«Ah, wir sind anscheinend zurück zu unseren verschlüsselten Wörtern.»",
              "Neben den vertrauten verschlüsselten Wörtern und Schlüssel findest du noch in kleinerer Schrift geschrieben: ",
              "In der Vielfalt der Spalten liegt die Stärke des Ganzen",
              "«Hm, das muss wohl ein Hinweis sein, um welche Art Rätsel es sich handelt»",
              "Du blätterst weiter im Tagebuch, bis du eine Seite findest, wo das Wort 'Spalte' öfters hervorgehoben ist.",
              "«Alles klar nach den Anweisungen muss ich: ",
              "1. Die Spaltenlänge bestimmen indem ich die Länge der verschlüsselten Nachricht durch die Länge des Schlüsselwortes teile.",
              "2. Die verschlüsselte Nachricht spaltenweise aufschreiben. Die Länge des Schlüssels bestimmt dabei die Anzahl der Spalten.",
              "3. Das Schlüsselwort alphabetisch sortiere und dann buchstabenweise über die Spalten schreibe.",
              "4. Die Spalten umsortiere, sodass die Buchstaben wieder das Schlüsselwort ergeben.",
              "5. Zum Schluss muss ich dann nurnoch zeilenweise die entschlüsselte Nachricht ablesen und voilà",
              "Ah und ich darf hier natürlich nicht vergessen, dass Abstände wichtig sind.»"
            ]
        },
   StorySecretItem
      StorySecret
        { storySecretText =
            [ "______________",
              "$SECRET",
              "Spaltentausch Chiffre, 'bald",
              "______________"
            ],
          storySecret = "Preis in Sicht",
          storyCypherFunction = (`columnarTranspositionCipher` "bald")
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "«Ich glaube nach diesem Abenteuer kann ich mich nicht nur als Besitzer der Weltformel betiteln, sondern auch als Rätselmeister.",
              "Es kann jedoch nicht mehr weit sein! Es ist nurnoch eine Seite im Tagebuch übrig.»",
              "Du gehst durch die Tür und findest dich in einem neuem Raum wieder."
            ]
        },
    WaitForEnterKeyItem,
    RoomItem
      RoomElement
        { roomName = "room_upper1"
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "«Das muss es sein! Das letzte Rätsel!»",
              "Eilig untersuchst du die Tür.",
              "«Neben einem kryptischen Text findest du noch einen weiteren Hinweis:",
              "'Denke zurück an die Anfänge deiner Reise. Die Lösung ist dort versteckt.'",
              "«Die Anfänge meiner Reise? Was kann das bloß bedeuten...»",
              "Du blätterst im Tagebuch zurück.",
              "«Natürlich! Das muss es sein! Mit Anfängen meint er die Anfänge der Rätselwörter. Füge ich diese zusammen ergibt sich bestimmt der Schlüssel!»",
              "Du blätterst hektisch auf die letzte Seite des Tagebuches zurück.",
              "«Danach muss ich nurnoch buchstabenweise das Schlüsselwort um die Position des Buchstaben des Schlüssels zurückverschieben;",
              "also den ersten Buchstaben des Schlüsselwortes mit dem ersten Buchstaben des Schlüssels und so weiter.»",
              "«Da der Schlüssel kürzer ist als das Wort, muss ich den Schlüssel nur sooft wiederholen bis er ausreicht.»"
            ]
        },
    StorySecretItem
      StorySecret
        { storySecretText =
            [ "______________",
              "$SECRET",
              "Vigenère-Chiffre, 'WEITP'",
              "______________"
            ],
          storySecret = "hallo",
          storyCypherFunction = (`vigenereCipher` "WEITP")
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "Endlich, dort auf dem Podest ist die Weltformel.",
              "Du kannst es kaum glauben, aber die Geschichte des großen Meisters stimmt wirklich.",
              "Langsam bewegst du dich auf das Podest zu."
            ]
        },
    RoomItem
      RoomElement
        { roomName = "room_weltformel"
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "Nur noch ein Schritt entfernt. Da liegt sie.",
              "Doch was ist das?",
              "",
              "«Das ist das geheime Rezept meiner Omi. Ich liebe meine Omi über alles ♥",
              "Omis Geheimer Milchkuchen",
              "",
              "🥛🥛🥛",
              "🍚🥚🍚  =  🎂",
              "🌾🌾🌾",
              "",
              "Zutaten:",
              "- 3 Eimer Milch",
              "- 2 mal Zucker",
              "- 1 Ei",
              "- 3 mal Weizen",
              "",
              "Dauer: 1 Stunde",
              "",
              "Anleitung:",
              "1. Die 3 Eimer Milch in einen großen Topf gießen und langsam erhitzen, bis sie warm, aber nicht kochend sind.",
              "2. In einer Schüssel den Weizen sieben und beiseite stellen.",
              "3. Das Ei in einer separaten Schüssel leicht verquirlen.",
              "4. Nach und nach den Zucker zur warmen Milch geben und gut umrühren, bis der Zucker vollständig aufgelöst ist.",
              "5. Das verquirlte Ei in die Milch-Zucker-Mischung einrühren. Ständig rühren, um sicherzustellen, dass das Ei gleichmäßig verteilt wird.",
              "6. Den gesiebten Weizen nach und nach zur Mischung hinzufügen, dabei kontinuierlich rühren, um Klumpen zu vermeiden. Rühren, bis ein glatter Teig entsteht.",
              "7. Den Backofen auf 180 Grad Celsius vorheizen.",
              "8. Eine Backform einfetten und den Teig gleichmäßig darin verteilen.",
              "9. Den Kuchen im vorgeheizten Ofen etwa 30-40 Minuten backen oder bis er goldbraun und durchgebacken ist.",
              "10. Nach dem Backen den Kuchen abkühlen lassen und nach Belieben mit Puderzucker bestreuen.",
              "Genieße Omis Geheimen Milchkuchen mit einer Tasse Tee oder Kaffee und erinnere dich an die liebevolle Tradition deiner Omi!"
            ]
        },
    WaitForEnterKeyItem,
    StoryTextItem
      StoryText
        { storyTextText =
            [ "",
              "Das kann nicht wahr sein. Ein Kuchenrezept?",
              "Du fällst auf die Knie und fängst an zu weinen.",
              "Doch immerhin kennst du jetzt Omis Geheimen Milchkuchen."
            ]
        },
    StoryTextItem
      StoryText
        { storyTextText =
            [ "  _____              _           __ _   _      _        _____       _      _            \n\
              \ |  __ \\            | |         / _(_) (_)    ( )      / ____|     (_)    | |           \n\
              \ | |  | | __ _ _ __ | | _____  | |_ _   _ _ __|/ ___  | (___  _ __  _  ___| | ___ _ __  \n\
              \ | |  | |/ _` | '_ \\| |/ / _ \\ |  _| | | | '__| / __|  \\___ \\| '_ \\| |/ _ \\ |/ _ \\ '_ \\ \n\
              \ | |__| | (_| | | | |   <  __/ | | | |_| | |    \\__ \\  ____) | |_) | |  __/ |  __/ | | |\n\
              \ |_____/ \\__,_|_| |_|_|\\_\\___| |_|  \\__,_|_|    |___/ |_____/| .__/|_|\\___|_|\\___|_| |_|\n\
              \                                                             | |                        \n\
              \                                                             |_|                        \n"
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

lengthOfGameLoopElements :: Int
lengthOfGameLoopElements = length gameLoopElements