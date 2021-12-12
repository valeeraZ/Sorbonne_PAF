module QuickCheckNiveau where

import Test.Hspec
import Test.QuickCheck

import Coord
import Niveau
import System.Random

--generateur du Niveau
genNiveau :: Gen Niveau 
genNiveau = do 
    seed <- choose (0, 1000)
    let gen = mkStdGen seed 
    return (generateNiveauVerified gen)
    
instance Arbitrary Niveau where
    arbitrary = genNiveau

instance Arbitrary Coord where
    arbitrary =  do
        x <- choose (1,10)
        y <- choose (1,10)
        return (C x y)

--propriete du generateur du Niveau
prop_genNiveau_inv :: Property 
prop_genNiveau_inv = forAll genNiveau $ prop_niveauInvariant


niveauSpec = do
  describe "Niveau générateur QuickCheck invariant" $ do
    it "Teste l'invariant pour les niveaux générées aléatoirement" $ 
      property prop_genNiveau_inv