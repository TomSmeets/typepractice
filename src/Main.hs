{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe
import Data.Time.Clock
import System.Console.ANSI
import System.Environment
import System.IO
import TypePractice
import Zipper

main :: IO ()
main = do
    stdin2 <- openFile "/dev/tty" ReadMode -- open a second stdin
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetBuffering stdin2 NoBuffering
    args <- getArgs
    case args of
        []  -> getContents >>= \str -> length str `seq` start stdin2 str
        [f] -> readFile f >>= start stdin2
        _   -> error "invalid amount of arguments"

-- | Start the type game
start :: Handle -- ^ text input handle
      -> String -- ^ text to practice
      -> IO ()
start inH txt' = do
    let txt = init . init . unlines . map (++ " ") . lines $ reformat 71 txt'
    hSetEcho inH False
    showCursor
    setCursorPosition 0 0
    clearScreen
    putStrLn txt
    setCursorPosition 0 0
    TypeSession{..} <- loop (TypeSession Nothing (fromList2 (lines txt)) 0)
    endTime <- getCurrentTime
    let speed = wpm (length $ words txt) (diffUTCTime endTime (fromJust startTime))
    putStrLn ""
    putStrLn $ "WPM: " ++ show speed
    putStrLn $ "Errors: " ++ show errors
  where
    loop s = do
        c <- hGetChar inH
        (s', d) <- typeChar c s
        if d then return s' else loop s'

typeChar :: Char -> TypeSession -> IO (TypeSession, Bool)
typeChar '\DEL' s = do
    let t' = left2 $ text s
    let (y, x) = idx2 t'
    setCursorPosition y x
    putChar (cursor2 t')
    setCursorPosition y x
    return (s{text = t'}, False)
typeChar c TypeSession{..} = do
    st <- case startTime of
        Nothing -> Just <$> getCurrentTime
        Just t  -> return $ Just t

    let madeError = cursor2 text /= c
    let errors' = errors + if madeError then 1 else 0

    let color = if madeError then wrong else normal
    putStr $ withColor color [cursor2 text]

    if isLast2 text
      then return (TypeSession st text errors', True)
      else (do
        let text' = right2 text
        let (y, x) = idx2 text'
        setCursorPosition y x
        return (TypeSession st text' errors', False))

-- | Give the string a color or effect with 'SGR'
withColor :: SGR -> String -> String
withColor col w = setSGRCode [col] ++ w ++ setSGRCode [Reset]

-- | Preset text colors
normal, wrong, highlighted :: SGR
normal      = Reset
wrong       = SetColor Background Vivid Red
highlighted = SetColor Background Vivid Blue
