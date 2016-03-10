module Main where

import System.Console.ANSI
import System.IO
import System.Environment
import Control.Monad
import Data.Time.Clock
import Data.Ratio

main :: IO ()
main = do
    stdin2 <- openFile "/dev/tty" ReadMode -- open a second stdin
    hSetBuffering stdin NoBuffering
    hSetBuffering stdin2 NoBuffering
    args <- getArgs
    case args of
        []  -> getContents >>= \str -> length str `seq` play stdin2 str
        [f] -> playFromFile f
        _   -> error "invalid amount of arguments"

segments n [] = []
segments n xs = take n xs : segments n (drop n xs)

playFromFile f = readFile f >>= play stdin

play h str = do
    clearScreen
    startT <- getCurrentTime
    mapM_ (play' h startT . unlines) . segments 8 . lines $ str
    endT   <- getCurrentTime
    let diff    = diffUTCTime endT startT
    let (m, s)  = (round diff) `divMod` 60
    let words'  = length (words str)
    putStrLn $ "words: " ++ show words'
    putStrLn $ "time: "  ++ show m ++ "m " ++ show s ++ "s"
    putStrLn $ "WPM: "   ++ show (round $ fromIntegral words' / (diff / 60))

wpm :: Integral a => UTCTime -> UTCTime -> a -> a
wpm start end words = round $ fromIntegral words / diffMin
  where
    diffMin = diffUTCTime end start / 60

play' h startT str = go "" >> putStrLn "DONE"
  where
    go xs | xs == str = return ()
          | otherwise = do
        now <- getCurrentTime
        display str xs
        nextInput h xs >>= go


nextInput h xs = do
    c <- hGetChar h
    case c of
        '\DEL' -> return $ init xs
        _      -> return $ xs ++ [c]

display str input = do
    let out = concat $ zipWith withColor (toColor str input) (map makeVisible str)
    setCursorPosition 0 0
    clearFromCursorToScreenEnd
    putStrLn out
  where
    withColor col w = setSGRCode [col] ++ w ++ setSGRCode [Reset]
    toColor (x:xs) (y:ys) | x == y    = highlighted : toColor xs ys
                          | otherwise = wrong       : toColor xs ys
    toColor [] (y:ys) = normal : toColor [] ys
    toColor (x:xs) [] = normal : toColor xs []
    toColor [] []     = []

makeVisible '\n' = "\\n\n"
makeVisible c    = [c]

normal      = SetColor Background Vivid Magenta
wrong       = SetColor Background Vivid Red
highlighted = SetColor Background Vivid Blue
