import Data.Char
import Story
import Cipher

main = do
    tellStory storyEinleitung
    input <- getLine
    putStrLn $ if input == "Willkommen im Spiel" then "Richtig" else "Falsch"
