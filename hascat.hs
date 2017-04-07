-- Concatenates and rainbowifies files.
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Random
import Control.Monad

-- Let handleArgs decide on what to do.
main :: IO ()
main = handleArgs =<< execParser opts
  where 
    opts = info sample
      fullDesc


-- An array containing Red, Green and Blue values.
type Color = [Int]

-- ANSI Escape Sequence.
type AES = String

data Sample = Sample
  { showHelp    :: Bool,
    showVersion :: Bool,
    freq        :: Int,
    seed        :: Int,
    offset      :: Int,
    invert      :: Bool,
    files       :: [String] }

-- The argument parser.
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
      <*> option auto
          ( long "offset"
         <> short 'O'
         <> value 2 
         <> metavar "<o>" )
      <*> switch
          ( long "invert"
         <> short 'i' )
      <*> many (argument str (metavar "FILE")) -- 0 or more file arguments.

-- Decodes arguments and decides on what to do.
handleArgs :: Sample -> IO ()
handleArgs (Sample True _ f s o i _) = rainbowPrint f s o i helpText
handleArgs (Sample _ True f s o i _) = rainbowPrint f s o i $ "hascat " ++ version
handleArgs (Sample _ _ f s o i [])   = do 
  stdin <- getContents
  rainbowPrint f s o i stdin
handleArgs (Sample _ _ f s o i fs)   = do
  fileContents <- traverse readFile' fs 
  rainbowPrint f s o i $ concat fileContents

-- Rainbowifies the input and prints to stdout.
-- f: the color frequency
-- s: the lines in the input
rainbowPrint :: Int -> Int -> Int -> Bool -> String -> IO ()
rainbowPrint f s o i input = do
  gen <- if s == 0 then getStdGen else return $ mkStdGen s
  when i $ putStr "\ESC[7m" -- Invert color
  putStr $ unlines $ rainbowLns (rand gen) f o $ lines input
  putStr reset
    where rand :: StdGen -> Int
          rand g = fst (randomR (0, 257) g) -- Takes the StdGen and generates a number.

-- Reads stdin on "-"
readFile' :: String -> IO String
readFile' "-"  = getContents 
readFile' file = readFile file

-- Inserts rainbow escape sequences into a line.
-- Takes three arguments:
-- c: the color to start with
-- f: how far to step in the colors array for every character
-- s: the line to rainbowify
rainbowLn :: Int -> Int -> String -> String
rainbowLn c f s = concat $ zipWith (:) s [ getEscape e | e <- [c,(c+f)..((length s * f) + c)] ]

-- Applies ranbowLn to multiple lines.
-- Takes four arguments:
-- c: the color to start with
-- f: how far to step in the colors array for every character
-- h: how far to step in the colors array for every line
-- ls: the lines to rainbowify 
rainbowLns :: Int -> Int -> Int -> [String] -> [String]
rainbowLns _ _ _ [] = []
rainbowLns c f o ls = (getEscape c ++ rainbowLn (c+f) f (head ls)) : rainbowLns (c+o) f o (tail ls)

-- Function to get an AES from the escapes list.
-- abs is needed to avoid negative index errors when vfreq is negative.
getEscape :: Int -> AES
getEscape n = escapes !! abs n

-- Infinitely long list containing all the colors of the rainbow.
escapes :: [AES]
escapes = cycle $ map (cStr . colors) [0..257]

-- Calculates Color 0-257 in the rainbow.
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

-- AES to reset the color to the default.
reset :: AES
reset = "\ESC[0m"
        
-- Constructs the AES for a given Color.
-- c: an array containing the values (0-255) for red, green and blue
cStr :: Color -> AES
cStr [r, g, b] = "\ESC[38;2;" ++ show r ++ ';':(show g ++ ';':(show b ++ "m"))
cStr c         = error $ "cStr: invalid Color: " ++ show c

-- Version number.
version :: String
version = "1.5.0"

-- Help text.
helpText :: String
helpText = unlines [ "Usage: hascat [-h|--help] [-v|--version] [-F|--freq <f>] [-S|--seed <s>]",
                     "              [-O|--offset <o>] [-i|--invert] [FILE]",
                     "",
                     "Concatenate files or standard input to standard output.",
                     "With no FILE, read standard input.",
                     "The FILE \"-\" represents stardard input.",
                     "",
                     "  -F, --freq <f>        Rainbow frequency (default: 2)",
                     "  -O, --offset <o>      Vertical offset (default: 2)",
                     "  -S, --seed <s>        RNG seed, 0 means random (default: 0)",
                     "  -i, --invert          Invert output",
                     "  -h, --help            Print this help message",
                     "  -v, --version         Print version information" ]
