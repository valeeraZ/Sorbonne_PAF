module Stats(
    countChars,
    countWords,
    countLines,
    countLetter,
    freqs,
    printList,
    topFreq
) where

import Data.Text (Text)

import qualified Data.Text as T

import Data.List (sortBy)

countChars :: Text -> Int
countChars = T.foldl' (\x _ -> x + 1) 0 

countWords :: Text -> Int 
countWords = T.foldl' (\x w -> if w == ' ' then x + 1 else x) 0

countLines :: Text -> Int 
countLines = T.foldl' (\x w -> if w == '\n' then x + 1 else x) 1

countLetter :: Char -> Text -> Int 
countLetter letter = T.foldl' (\x w -> if w ==letter then x + 1 else x) 0

makeCounter :: Char -> (Text -> Int)
makeCounter c  = countLetter c

counters :: [Text -> Int]
counters = map makeCounter ['a'..'z']

frequencies :: [Text -> Int] -> Text -> [Int]
frequencies [] _ = []
frequencies (f:fs) cs = (f cs) : frequencies fs cs

freqs :: Text -> [(Char, Int)]
freqs cs = zip ['a'..'z'] (frequencies counters cs)

topFreq :: [(Char, Int)] -> [(Char, Int)]
topFreq freqs = take 10 (sortBy (\(_,a) (_,b) -> compare b a) freqs)

printList :: [(Char, Int)] -> String
printList [] = []
printList ((c, i):frs) = ((c:[]) ++ ": " ++ (show i) ++ "\n") ++ printList frs