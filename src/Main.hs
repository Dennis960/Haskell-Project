import Story (storyEinleitung, tellStory)

main = do
  tellStory storyEinleitung
  input <- getLine
  putStrLn $ if input == "Willkommen im Spiel" then "Richtig" else "Falsch"
