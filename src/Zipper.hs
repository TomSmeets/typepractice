{-# LANGUAGE DeriveFunctor #-}
module Zipper where

import Test.QuickCheck

data Zipper a = Zip { lefts :: [a], cursor :: a, rights :: [a] } deriving (Show, Functor, Eq)

instance Arbitrary a => Arbitrary (Zipper a) where
    arbitrary = Zip <$> arbitrary <*> arbitrary <*> arbitrary

left, right :: Zipper a -> Zipper a
left  (Zip (l:ls) x rs) = Zip ls l (x:rs)
left  _                 = error "Zipper.left: Can't go left if at the beginning of the list!"
right (Zip ls x (r:rs)) = Zip (x:ls) r rs
right _                 = error "Zipper.right: Can't go right if at the end of the list!"

-- | Check if we are at the left end of the list
isFirst :: Zipper a -> Bool
isFirst = null . lefts

-- | Check if we are at the right end of the list
isLast :: Zipper a -> Bool
isLast  = null . rights

-- | Get our current the index in the list
-- prop> \zl -> cursor zl == toList zl !! idx zl
idx :: Zipper a -> Int
idx = length . lefts

-- | Convert beteen a list and a 'Zipper'
-- prop> \xs -> not (null xs) ==> (toList $ fromList xs) == xs
fromList :: [a] -> Zipper a
fromList (x:xs) = Zip [] x xs
fromList []     = error "Zipper.fromList: a zipper can not be empty!"

toList :: Zipper a -> [a]
toList (Zip ls x rs) = reverse ls ++ [x] ++ rs

mapCursor :: (a -> a) -> Zipper a -> Zipper a
mapCursor f (Zip ls x rs) = Zip ls (f x) rs


-- | 2D Zipper
type Zipper2 a = Zipper (Zipper a)

cursor2 :: Zipper2 a -> a
cursor2 = cursor . cursor

fromList2 :: [[a]] -> Zipper2 a
fromList2 = fromList . map fromList

left2 :: Zipper2 a -> Zipper2 a
left2 t | isFirst (cursor t) = left t
        | otherwise          = mapCursor left t

right2 :: Zipper2 a -> Zipper2 a
right2 t | isLast (cursor t) = right t
         | otherwise         = mapCursor right t

idx2 :: Zipper2 a -> (Int, Int)
idx2 z = (idx z, idx $ cursor z)

isFirst2 :: Zipper2 a -> Bool
isFirst2 z = isFirst z && isFirst (cursor z)

isLast2 :: Zipper2 a -> Bool
isLast2 z  = isLast z && isLast (cursor z)
