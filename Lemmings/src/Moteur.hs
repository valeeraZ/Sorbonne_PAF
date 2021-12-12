module Moteur where

import Etat
import Control.Concurrent
import System.Console.ANSI
import Linear
import Keyboard


tourMoteur :: Int -> Etat -> Maybe (V2 Int) -> Keyboard -> IO (Either Fin (Int,Etat))
tourMoteur n e coordinates kbd = case tourEtat n e coordinates kbd of 
                    Left f -> return $ Left f
                    Right ne -> do
                                --print ne
                                --putStrLn ("Tour " <> show n <> " restants : " <> show (lrestantsE ne) <> ", vivants : " <> show (lvivantsE ne))
                                --putStrLn ""
                                threadDelay 50000
                                --clearScreen
                                return $ Right (n+1,ne)
