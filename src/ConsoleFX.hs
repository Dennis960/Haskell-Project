module ConsoleFX (staticForSeconds) where

import Control.Concurrent (threadDelay)

grayscale :: String
grayscale = "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "

targetFps :: Integer
targetFps = 60

-- | prints a list of string to the console
printScreen :: [String] -> IO ()
printScreen screen = do
  putStrLn (init (unlines screen))
  threadDelay
    (round (1000000 / fromIntegral targetFps))

-- | clears lines in the console corresponding to the number of lines in the given screen
clearScreen :: [String] -> IO ()
clearScreen screen = putStr ("\ESC[" ++ show (length screen) ++ "A")

-- | generates a pseudo-random number from a given seed
pseudoRandom :: Int -> Int
pseudoRandom seed = (a * (seed `mod` m) + c) `mod` m
  where
    a = 1103515245
    c = 12345
    m = 2 ^ 31

-- | generates a screen of random characters with the given width and height for the given screen number
-- a different screen number will result in a completely different screen
randomScreen :: Int -> Int -> Int -> [String]
randomScreen width height screenNumber = [randomString width ((screenNumber * width * height) + width * i) | i <- [1 .. height]]
  where
    randomString :: Int -> Int -> String
    randomString length seed = [randomChar (seed + i) | i <- [1 .. length]]
    randomChar :: Int -> Char
    randomChar seed = grayscale !! (pseudoRandom seed `mod` length grayscale)

-- | quickly prints a box of random characters giving the apperance of a static screen with the given width and height for the given number of seconds
staticForSeconds :: Int -> Int -> Float -> IO ()
staticForSeconds width height seconds = staticForTimes width height (round (seconds * fromIntegral targetFps))
  where
    staticForTimes :: Int -> Int -> Int -> IO ()
    staticForTimes width height 0 = printScreen (randomScreen width height 0)
    staticForTimes width height times = do
      let screen = randomScreen width height times
      printScreen screen
      clearScreen screen
      staticForTimes width height (times - 1)
