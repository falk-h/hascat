-- |Concatenates and rainbowifies files.
module Main where

import System.Environment
import System.Random

-- |An array containing Red, Green and Blue values.
type Color = [Int]

-- |ANSI Escape Sequence
type AES = String

-- |Splits a line into Strings of a specific length.
-- The last returned String may be shorter than the others
-- Takes two arguments:
-- n: how long the split strings should be
-- s: the string to split
-- splitN :: Int -> String -> [String]
-- splitN _ [] = []
-- splitN n s | n >= length s = [s]
--           | otherwise = take n s : splitN n (drop n s)

-- |Default color frequency is 2
defaultFrequency :: Int
defaultFrequency = 2

-- |Version number
version :: String
version = "1.0.1"

-- |Main
main :: IO ()
main = do 
  args <- getArgs
  case args of
    _ | "-v" `elem` args -> printVersion
    _ | "--version" `elem` args -> printVersion
    _ | "-h" `elem` args -> printUsage
    _ | "--help" `elem` args -> printUsage
    ["-F",freq] | [(frequency, _)] <- reads freq -> do
      stdin <- getContents
      rainbowPrint frequency stdin
    [] -> do
      stdin <- getContents
      rainbowPrint defaultFrequency stdin
    "-F":freq:files -> do
      fileContents <- mapM readFile files 
      rainbowPrint (read freq) $ concat fileContents
    files -> do
      fileContents <- mapM readFile files 
      rainbowPrint defaultFrequency $ concat fileContents

-- |Passes version information to rainbowPrint
printVersion :: IO ()
printVersion = do
  name <- getProgName
  rainbowPrint defaultFrequency $ name ++ " " ++ version

-- |Passes usage information to rainbowPrint
printUsage :: IO ()
printUsage = do
  name <- getProgName
  rainbowPrint defaultFrequency $ unlines ["Usage: " ++ name ++ " [OPTION]... [FILE]...",
                                        "",
                                        "Concatenate files or standard input to standard output.",
                                        "With no FILE, read standard input.",
                                        "",
                                        "  -h, --help\t\tPrint this help message",
                                        "  -v, --version\t\tPrint version information"]

-- |Rainbowifies the input and prints to stdout
-- f: the color frequency
-- s: the lines in the input
rainbowPrint :: Int -> String -> IO ()
rainbowPrint f s = do
  rand <- getStdRandom (randomR (0, 257))
  putStr $ unlines $ rainbowLns rand f $ lines s
  putStr reset

-- |Inserts rainbow escape sequences into a line
-- Takes three arguments:
-- o: the number of characters betweeen changing colours
-- c: the next colour of the rainbow to insert (0-255)
-- s: the line to rainbowify
rainbowLn :: Int -> Int -> String -> String
rainbowLn c f s = concat $ zipWith (:) s [ getEscape e | e <- [c,(c+f)..((length s * f) + c)] ]

-- |Applies ranbowLn to multiple lines.
-- Takes four arguments:
-- c: The starting colour
-- vo: the vertical offset
-- ho: the horizontal offset
-- ls: the lines to rainbowify 
rainbowLns :: Int -> Int -> [String] -> [String]
rainbowLns _ _ [] = []
rainbowLns c f ls = (getEscape c ++ rainbowLn (c+f) f (head ls)) : rainbowLns (c+f) f (tail ls)

-- |Function to get an AES from the escapes list
getEscape :: Int -> AES
getEscape n = escapes !! n

-- |Infinitely long list containing the colors of the rainbow.
escapes :: [AES]
escapes = cycle $ map (cStr . colors) [0..257]

-- |Gets Color 0-257 in the rainbow
colors :: Int -> Color
colors n | h == 0 = [255, t, 0]
         | h == 1 = [q, 255, 0]
         | h == 2 = [0, 255, t]
         | h == 3 = [0, q, 255]
         | h == 4 = [t, 0, 255]
         | h == 5 = [255, 0, q]
         | otherwise = error $ "colors: can't get color for " ++ show n
         where 
           h = div n 43
           f = n-43*h
           t = div (f*255) 43
           q = 255-t

-- | AES to reset the color to the default.
reset :: AES
reset = "\ESC[0m"
        
-- |Gets the AES for a given Color.
cStr :: Color -> AES
cStr [r, g, b] = "\ESC[38;2;" ++ show r ++ ';':(show g ++ ';':(show b ++ "m"))
cStr c         = error $ "cStr: invalid Color: " ++ show c
