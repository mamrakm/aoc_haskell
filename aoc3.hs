{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import qualified Data.List as L
data MapElem = BuildingBlock { tree :: Char, plain :: Char }

charWithPosition :: String -> (Char, (Int, Int))
charWithPosition = error "Unimplemented"
main = withFile "./aoc3.data" ReadMode listFile
    where listFile h = hGetContents h >>= addPositionsToSymbols

erathostenes [] = []
erathostenes (p : s) = [n | n <- p : erathostenes (Prelude.filter multiplies s)]
                        where multiplies = \i -> i `mod` p /= 0
t :: T.Text
t = "Kokutek"

data Fetoš = Fetoš { name :: String, favouriteFet :: String, dailyDose :: Int, fetoshLvl :: Integer } deriving (Show)
addPositionsToSymbols :: String -> [(Char, (Integer, Integer))]
addPositionsToSymbols [] = []
addPositionsToSymbols (c:s)= L.foldl incr [(c, (0, 0))] s
                where incr acc e = let pos = snd $ head acc in (e, (incrX pos , incrY pos e)) : acc
                      incrX pos = fst pos + 1
                      incrY pos e = if e == '\n' then snd pos + 1 else snd pos