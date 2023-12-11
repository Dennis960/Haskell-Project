module Introduction
  ( playIntroduction,
  )
where

import Control.Concurrent (threadDelay)
import KeyEvents (disableInputEcho, enableInputEcho)
import System.IO (hFlush, stdout)
import Typer (clearScreenMoveHome)

title :: String
title =
  "   _____ _       _                 __  __           _                     \n\
  \  / ____(_)     | |               |  \\/  |         | |                    \n\
  \ | |     _ _ __ | |__   ___ _ __  | \\  / | __ _  __| |_ __   ___  ___ ___ \n\
  \ | |    | | '_ \\| '_ \\ / _ \\ '__| | |\\/| |/ _` |/ _` | '_ \\ / _ \\/ __/ __|\n\
  \ | |____| | |_) | | | |  __/ |    | |  | | (_| | (_| | | | |  __/\\__ \\__ \\\n\
  \  \\_____|_| .__/|_| |_|\\___|_|    |_|  |_|\\__,_|\\__,_|_| |_|\\___||___/___/\n\
  \          | |                                                             \n\
  \          |_|                                                             "

-- | Prints a progress bar of type [####    ]
printProgressBarPercentage :: Float -> Int -> IO ()
printProgressBarPercentage percentage width = do
  putStr "["
  putStr $ replicate (round (percentage * fromIntegral width)) '#'
  putStr $ replicate (width - round (percentage * fromIntegral width)) ' '
  putStr "]"
  hFlush stdout

printProgress :: [Float] -> Float -> IO ()
printProgress [] _ = return ()
printProgress (delay : delays) progress = do
  clearScreenMoveHome
  putStr "Compiling Story "
  printProgressBarPercentage progress 20
  putStrLn ""
  threadDelay (round (delay * 500000))
  printProgress delays (progress + 0.1)

-- | Plays the introduction sequence simulating a loading bar
playIntroduction :: IO ()
playIntroduction = do
  disableInputEcho

  let delays = [1, 0.3, 0.2, 0.5, 2, 0.1, 0.1, 0.1, 0.05, 0.5, 0.2]
  printProgress delays 0
  putStrLn "\nDone!\nWelcome to:"
  putStrLn title
  enableInputEcho
