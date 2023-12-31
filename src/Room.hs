module Room
  ( Room,
    loadRoom,
    printRoom,
    loopPlayerInsideRoom,
  )
where

import ConsoleFX (staticForSeconds)
import KeyEvents (Direction (..), getDirectionKey)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data CellType = WALL | STORY | PLAYER_INIT | EMPTY | TRAP deriving (Eq)

data Cell = Cell
  { cellType :: CellType,
    cellPosition :: Position
  }
  deriving (Show)

type Position = (Int, Int)

instance Read CellType where
  readsPrec _ value = case value of
    "█" -> [(WALL, "")]
    "?" -> [(STORY, "")]
    "T" -> [(TRAP, "")]
    "P" -> [(PLAYER_INIT, "")]
    _ -> [(EMPTY, "")]

instance Show CellType where
  show cell = case cell of
    WALL -> "█"
    STORY -> "?"
    PLAYER_INIT -> " "
    TRAP -> " "
    EMPTY -> " "

data Room = Room
  { roomCells :: [[Cell]],
    playerPosition :: Position
  }
  deriving (Show)

playerSymbol = "P"

-- | Returns the room with the given name from rooms/
loadRoom :: String -> IO Room
loadRoom a = do
  let path = "rooms/" ++ a
  handle <- openFile path ReadMode
  roomString <- hGetContents handle
  let cells = parseRoom roomString
  -- find the player position
  let playerPosition = findPlayerPosition cells
  return $ Room cells playerPosition
  where
    parseRoom :: String -> [[Cell]]
    parseRoom roomString = zipWith (curry parseRow) [0 ..] (lines roomString)
    parseRow :: (Int, String) -> [Cell]
    parseRow (y, rowString) = zipWith (curry (parseCell . (\(x, cellChar) -> (x, y, cellChar)))) [0 ..] rowString
    parseCell :: (Int, Int, Char) -> Cell
    parseCell (x, y, cellChar) = Cell (read [cellChar]) (x, y)

    findPlayerPosition :: [[Cell]] -> Position
    findPlayerPosition cells = case filter (\cell -> cellType cell == PLAYER_INIT) (concat cells) of
      [cell] -> cellPosition cell
      _ -> error "There must be exactly one player position in the room"

-- | Prints the given room to the console. The player is represented by the playerSymbol.
printRoom :: Room -> IO ()
printRoom room = do
  putStr (unlines (map printRow (roomCells room)))
  where
    printRow :: [Cell] -> String
    printRow = concatMap printCell
    printCell :: Cell -> String
    printCell cell = if cellPosition cell == playerPosition room then playerSymbol else show (cellType cell)

-- | Clears as many lines in the console as there are rows in the given room.
clearRoom :: Room -> IO ()
clearRoom room = putStr ("\ESC[" ++ show (length (roomCells room)) ++ "A")

-- | Moves the player in the given room in the given direction. If the player cannot move in that direction, the room is returned unchanged.
roomDirectionMovePlayer :: Room -> Direction -> Room
roomDirectionMovePlayer room@Room {playerPosition} direction =
  let newPlayerPosition = case direction of
        DirectionUp -> (fst playerPosition, snd playerPosition - 1)
        DirectionDown -> (fst playerPosition, snd playerPosition + 1)
        DirectionLeft -> (fst playerPosition - 1, snd playerPosition)
        DirectionRight -> (fst playerPosition + 1, snd playerPosition)
        DirectionNone -> playerPosition
   in case cellType (roomPositionGetCell room newPlayerPosition) of
        WALL -> room
        _ -> room {playerPosition = newPlayerPosition}

-- | Returns the cell at the given position in the given room.
roomPositionGetCell :: Room -> Position -> Cell
roomPositionGetCell room (x, y) = roomCells room !! y !! x

-- | Returns True if the player is touching a story cell in the given room.
isPlayerTouchingStory :: Room -> Bool
isPlayerTouchingStory room = cellType (roomPositionGetCell room (playerPosition room)) == STORY || cellType (roomPositionGetCell room (playerPosition room)) == TRAP

-- | Looping function that moves the player in the given room until the player is touching a story cell.
loopPlayerInsideRoom :: Room -> IO ()
loopPlayerInsideRoom room = do
  direction <- getDirectionKey
  let newRoom = roomDirectionMovePlayer room direction
  clearRoom room
  printRoom newRoom
  case cellType (roomPositionGetCell newRoom (playerPosition newRoom)) of
    TRAP -> do
      clearRoom newRoom
      let (roomWidth, roomHeight) = (length (head (roomCells room)), length (roomCells room))
      staticForSeconds roomWidth roomHeight 1
      return ()
    STORY -> return ()
    _ -> loopPlayerInsideRoom newRoom