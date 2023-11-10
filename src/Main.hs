import Cipher
import Data.Char
import Story

main = do
  tellStory storyEinleitung
  input <- getLine
  putStrLn $ if input == "Willkommen im Spiel" then "Richtig" else "Falsch"
