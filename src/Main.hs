import Story (tellStory, isStoryInputCorrect)

main = do
  tellStory 532
  input <- getLine
  putStrLn $ if isStoryInputCorrect 532 input then "Richtig" else "Falsch"
  main
