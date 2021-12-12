
module NiveauSpec where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq


import Test.Hspec
import Niveau

import Control.Exception (evaluate)
--un exemple de niveau
niveau1 :: Niveau 
niveau1 = read "XXXXXXXXXX\nX        X\nXE       X\nX        X\nX       0X\nX0       X\nX      00X\nX        X\nX0000    X\nX    00  X\nX    00 XX\nX       SX\nXXXXXXXXXX" 

niveauTest  = do
  describe "Tests sur la niveau" $ do
    it "Cases comprises dans la niveau" $ do
      prop_niveauFerme  niveau1 `shouldBe` True 
    it "Une entree dans le cordonne correcte" $ do
      prop_niveauEntreeCorrecte niveau1 `shouldBe` True 
    it "Une sortie dans le cordonne correcte" $ do
      prop_niveauSortieCorrecte niveau1 `shouldBe` True 
    it "Unique entree/sortie" $ do
      prop_niveauEntreeSortie niveau1 `shouldBe` True 
    it "Teste tous" $ do
      prop_niveauInclusion niveau1 `shouldBe` True 


engineSpec = do
  niveauTest
