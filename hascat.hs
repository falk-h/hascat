-- |Concatenates and rainbowifies files.
module Main where

import System.Environment
import System.Random

-- |An array containing Red, Green and Blue values.
type Color = [Int]

-- |ANSI Escape Sequence
type AES = String

-- |Splits a line into Strings of a specific length.
-- (Currently not used, though it will probably come in handy later)
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
version = "1.0.2"

-- |Main
-- Gets command line arguments, and decided what to do.
main :: IO ()
main = do 
  args <- getArgs
  case args of
    -- Print version number if -v or --version is somewhere in the args.
    _ | "-v" `elem` args -> printVersion
    _ | "--version" `elem` args -> printVersion
    -- Print usage help if -h or --help is somewhere in the args.
    _ | "-h" `elem` args -> printUsage
    _ | "--help" `elem` args -> printUsage
    -- Get the user defined frequency if the args are -F <f>, and read from stdin.
    ["-F",freq] | [(frequency, _)] <- reads freq -> do
      stdin <- getContents
      rainbowPrint frequency stdin
    -- Read from stdin if args are empty.
    [] -> do
      stdin <- getContents
      rainbowPrint defaultFrequency stdin
    -- Read from files with specified frequency if args start with -F <f>.
    "-F":freq:files -> do
      fileContents <- mapM readFile files 
      rainbowPrint (read freq) $ concat fileContents
    -- Otherwise, read from files.
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
                                        "  -F <f>\t\tRanbow frequency (default: 2)",
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
-- c: the color to start with
-- f: how far to step in the colors array for every character
-- s: the line to rainbowify
rainbowLn :: Int -> Int -> String -> String
rainbowLn c f s = concat $ zipWith (:) s [ getEscape e | e <- [c,(c+f)..((length s * f) + c)] ]

-- |Applies ranbowLn to multiple lines.
-- Takes four arguments:
-- c: the color to start with
-- f: how far to step in the colors array for every line
-- ls: the lines to rainbowify 
rainbowLns :: Int -> Int -> [String] -> [String]
rainbowLns _ _ [] = []
rainbowLns c f ls = (getEscape c ++ rainbowLn (c+f) f (head ls)) : rainbowLns (c+f) f (tail ls)

-- |Trivial function to get an AES from the escapes list
getEscape :: Int -> AES
getEscape n = escapes !! n

-- |Infinitely long list containing all the colors of the rainbow.
escapes :: [AES]
escapes = cycle $ map (cStr . colors) [0..257]

-- |Calculates Color 0-257 in the rainbow
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
        
-- |Constructs the AES for a given Color.
-- c: an array containing the values (0-255) for red, green and blue
cStr :: Color -> AES
cStr [r, g, b] = "\ESC[38;2;" ++ show r ++ ';':(show g ++ ';':(show b ++ "m"))
cStr c         = error $ "cStr: invalid Color: " ++ show c
