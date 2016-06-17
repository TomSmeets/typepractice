module TypePractice where

import Data.Time.Clock
import Zipper

data TypeSession = TypeSession { startTime :: Maybe UTCTime
                               , text      :: Zipper2 Char
                               , errors    :: Int
                               } deriving Show

-- | Reformat a string to a specific maximum length
--
-- >>> putStrLn $ reformat 16 "Hello, World. How are you today?"
-- Hello, World.
-- How are you
-- today?
--
-- It will never cut up words
-- >>> putStrLn $ reformat 4 "Hello, World. How are you today?"
-- Hello,
-- World.
-- How
-- are
-- you
-- today?
reformat :: Int -> String -> String
reformat m xs = f 0 (words xs)
  where
    f 0 (w:wx) = w ++ f (length w) wx
    f n (w:wx) | n + length w < m = " " ++ w ++ f (n + length w + 1) wx
               | otherwise = '\n':f 0 (w:wx)
    f _ [] = []

-- | Calculate the words per minute given the word count, start and end times
--
-- prop> \n -> wpm n 60 == n
wpm :: Integral a => a -> NominalDiffTime -> a
wpm n diff = round $ fromIntegral (n * 60) / diff
