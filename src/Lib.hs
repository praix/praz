module Lib
    ( someFunc
    ) where


import Data.Set as Set
import Data.Array

data DomElem = DomElem {tag :: String, tagId :: Maybe String, classes :: Set String}


class Dist a where
    dist :: a -> a -> Int
    cost :: a -> Int

instance Dist Char where
    dist a b = if a == b then 0 else 1
    cost _ = 1

editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table ! (m,n)
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
 
    table :: Array (Int,Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
 
    -- fix these 
    dist (0,j) = j
    dist (i,0) = i
    dist (i,j) = minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]

someFunc :: IO ()
someFunc = do
	putStrLn "someFunc"
