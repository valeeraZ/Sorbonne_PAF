module EtatSpec where

import Etat
import Niveau
import Environnement
import Test.Hspec

--un exemple de niveau
niveau1 :: Niveau 
niveau1 = read "XXXXXXXXXX\nX        X\nXE       X\nX        X\nX       0X\nX0       X\nX      00X\nX        X\nX0000    X\nX    00  X\nX    00 XX\nX       SX\nXXXXXXXXXX" 

envi1::Envi 
envi1 = envide  13 10

-- un exemple d'etat
e1::Etat 
e1 = (Etat envi1 niveau1 1 2 3 4)

etatTest = do
    describe "Tests Etat" $ do
        it "Etat initiale" $ do
            prop_etat e1 `shouldBe` True

engineSpec = do
  etatTest