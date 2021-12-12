module QuickCheckEntite where
import Environnement
import Test.Hspec
import Test.QuickCheck
import System.Random
import Lemming
import Coord


lemMarcheurG :: Lemming
lemMarcheurG = Marcheur Droite (C 2 1)

--exemple de lemming du marcheur
genEntiteMarcheurG :: Gen Entite 
genEntiteMarcheurG = do 
    seed <- choose (0, 20)
    return (Lem seed lemMarcheurG)

--exemple de lemming du creuseur
genEntiteCreuseur  :: Gen Entite 
genEntiteCreuseur  = do 
    seed <- choose (0, 20)
    return (Lem seed (Creuseur Droite (C 2 1)))


instance Arbitrary Entite where
  arbitrary =
    frequency [(2, genEntiteMarcheurG ) -- 20% de génération marcheur
              , (8, genEntiteCreuseur )] -- 80% de génération Creuseur
            
-- verifier le propriete du lemming
prop_genEntite_inv ::Entite -> Property 
prop_genEntite_inv e = property $ isMarcheurDroite e || isCreuseur e


entiteSpec = do
  describe "Entite générateur QuickCheck invariant" $ do
    it "Teste l'invariant pour les entites générées aléatoirement" $ 
      property prop_genEntite_inv