module OptionMenu where

import KeyEvents
  ( Direction (DirectionDown, DirectionNone, DirectionUp),
    disableInputEcho,
    enableInputEcho,
    getKey,
    keyToDirection,
  )
import Typer (clearLines)

underlineString :: String -> String
underlineString text = "\ESC[4m" ++ text ++ "\ESC[0m"

concatWithSeparator :: String -> [String] -> String
concatWithSeparator seperator = foldl1 (\left right -> left ++ seperator ++ right)

printOptions :: String -> Int -> [(a, String)] -> IO ()
printOptions message selectedIndex list = do
  putStrLn $ "\n" ++ message
  let optionStrings = zipWith (curry (\(index, (option, text)) -> if index == selectedIndex then underlineString (show index ++ " " ++ text) else show index ++ " " ++ text)) [1 ..] list
  putStrLn $ concatWithSeparator "\n" optionStrings

-- | prints a list of options to the console and returns the selected option
-- options can be selected with the arrow keys and confirmed with the enter key
selectOption :: String -> [(a, String)] -> IO a
selectOption message list = do
  disableInputEcho
  printOptions message 1 list
  selectedOption <- selectOption' message 1 list
  clearLines (length list + 2)
  enableInputEcho
  return selectedOption
  where
    -- \| helper function for selectOption, the first argument is the currently selected option
    selectOption' :: String -> Int -> [(a, String)] -> IO a
    selectOption' message selectedIndex list = do
      clearLines (length list + 2)
      printOptions message selectedIndex list
      key <- getKey
      case keyToDirection key of
        DirectionUp -> selectOption' message (max 1 (selectedIndex - 1)) list
        DirectionDown -> selectOption' message (min (length list) (selectedIndex + 1)) list
        DirectionNone -> case key of
          "\n" -> return $ fst (list !! (selectedIndex - 1))
          _ -> selectOption' message selectedIndex list
