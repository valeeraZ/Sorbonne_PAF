{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)

import qualified Data.Text as T

-- Les fonctions d'entrées sorties pour les textes
import qualified Data.Text.IO as TIO

import Stats ( countChars, countLines, countWords, countLetter, freqs, printList, topFreq)

main :: IO ()
main = do
    x <- TIO.readFile "pg9645.txt"
    TIO.putStrLn "Lecture de fichier pg9645.txt"
    putStrLn ("Nombre de caractères: " ++ show (countChars x))
    putStrLn ("Nombre de mots: " ++ show (countWords x))
    putStrLn ("Nombre de ligne: " ++ show (countLines x))
    putStrLn ("Les 10 lettres les plus fréquentes:\n" ++ printList (topFreq (freqs (T.toLower x))))