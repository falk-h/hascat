-- |Concatenates and rainbowifies files.
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Random

-- |Let handleArgs decide on what to do.
main :: IO()
main = handleArgs =<< execParser opts
  where 
    opts = info sample
      fullDesc


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
                     

data Sample = Sample
  { showHelp    :: Bool,
    showVersion :: Bool,
    freq        :: Int,
    seed        :: Int,
    files       :: [String] }

-- |The argument parser
sample :: Parser Sample
sample = Sample
      <$> switch
          ( long "help"
         <> short 'h' )
      <*> switch
          ( long "version"
         <> short 'v' )
      <*> option auto
          ( long "freq"
         <> short 'F'
         <> value 2 
         <> metavar "<f>" )
      <*> option auto
          ( long "seed"
         <> short 'S'
         <> value 0 
         <> metavar "<s>" )
      <*> many (argument str (metavar "FILE")) -- 0 or more file arguments

-- |Decodes arguments and decides on what to do.
handleArgs :: Sample -> IO ()

-- Print help text if -h or --help is passed
handleArgs (Sample True _ f s _)      = rainbowPrint f s helpText

-- Print version information if -v or --version is passed
handleArgs (Sample _ True f s _) = rainbowPrint f s $ "hascat " ++ version

-- Call rainbowPrint with stdin if we didn't get any files
handleArgs (Sample _ _ f s []) = do 
  stdin <- getContents
  rainbowPrint f s stdin

-- Call rainbowPrint with the concatenated file contents
handleArgs (Sample _ _ f s fs) = do
  fileContents <- traverse getInput fs 
  rainbowPrint f s $ concat fileContents

-- |Rainbowifies the input and prints to stdout
-- f: the color frequency
-- s: the lines in the input
rainbowPrint :: Int -> Int -> String -> IO ()
rainbowPrint f s input = do
  gen <- if s == 0 then getStdGen else return $ mkStdGen s
  putStr $ unlines $ rainbowLns (rand gen) f $ lines input
  putStr reset
    where rand :: StdGen -> Int
          rand g = fst (randomR (0, 257) g) -- Takes the StdGen and generates a number

-- |Reads stdin on "-"
getInput :: String -> IO String
getInput "-"  = getContents 
getInput file = readFile file

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

-- |Version number
version :: String
version = "1.3.0"

-- |Help text
helpText :: String
helpText = unlines [ "Usage: hascat [-h|--help] [-v|--version] [-F|--freq <f>] [-S|--seed <s>] [FILE]",
                     "",
                     "Concatenate files or standard input to standard output.",
                     "With no FILE, read standard input.",
                     "The FILE \"-\" represents stardard input.",
                     "",
                     "  -F, --freq <f>        Rainbow frequency (default: 2)",
                     "  -S, --seed <s>        RNG seed, 0 means random (default: 0)",
                     "  -h, --help            Print this help message",
                     "  -v, --version         Print version information" ]
