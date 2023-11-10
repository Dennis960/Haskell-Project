module Story where
import Control.Concurrent (threadDelay)
import System.IO

storyEinleitung = 532
defaultTypingDelay = 10000

putTextNl :: String -> IO ()
putTextNl [] = putStrLn "\n"
putTextNl (c:text) = do
    putStr [c]
    hFlush stdout -- flush the buffer, used to immediately print the character instead of waiting for a newline
    threadDelay defaultTypingDelay
    putTextNl text
tellStory :: Integer -> IO ()
tellStory 532 = do
    putTextNl "Du wachst auf. Du weißt nicht, wo du bist. Alles ist dunkel."
    putTextNl "An deinem Handgelenk klebt ein grüner Zettel. Du liest die Worte: »Wenn du dich weigerst, zu kooperieren, dann wirst du es nicht mehr hinausschaffen.«"
    putTextNl "Vor lauter Schock fällt dir das Handy aus der Hand. Es geht kaputt. Du bist allein. Niemand kann dir helfen. Du musst dich selbst retten."
    putTextNl "Nach einer Weile fällt dir leuchtende Schrift auf, doch du weißt nicht, was sie bedeutet."
    putTextNl "»Zlonrpphq lp Vslho«"
    putTextNl "Weiter unten steht ein Hinweis: »Schlüssel: 3. Caesar Shift. Viel Erfolg«"
    putTextNl "Plötzlich leuchtet ein Terminal auf. Du kannst einen Text eingeben. Neben dem Terminal steht eine Zahl: 532."
    putStr ">>"
tellStory _ = putStrLn "Es gibt keine Story für diese Zahl."