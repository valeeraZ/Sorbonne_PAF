module EnvSpec where
import Coord
import Environnement
import Lemming
import Test.Hspec

idd::Int 
idd = 4

coordd  :: Coord  
coordd = C 2 1

lem1 :: Entite
lem1 = Lem idd (Marcheur Droite coordd)

lem2 :: Entite
lem2 = Lem idd (Marcheur Droite coordd)

lem3 :: Entite
lem3 = Lem idd (Marcheur Droite coordd)

lem4 :: Entite
lem4 = Lem idd (Marcheur Droite coordd)

envi1::Envi 
envi1 = envide  13 10

--deplacer à droite
envi2 :: Envi
envi2 = deplaceDansEnvi idd (droite (droite coordd)) (addEntite lem2 envi1)

-- deplacer à droite puis en en haut
envi3 :: Envi
envi3 = deplaceDansEnvi idd (droite (haut coordd)) (addEntite lem3 envi1)

-- deplacer en haut puis à droite
envi4 :: Envi
envi4 = deplaceDansEnvi idd (haut (droite coordd)) (addEntite lem4 envi1)

-- add entite sur environnement
envi5 = addEntite  lem1  envi1
-- enleve entite sur environnement
envi6 = enleveEnvi idd envi5

enviTest = do
    describe "Tests environnement" $ do
        it "Environnement initiale" $ do
            prop_envi_inv envi1 `shouldBe` True
        it "Add entite" $ do
            prop_post_addEntite idd (addEntite lem1 envi1) `shouldBe` True
            prop_envi_inv envi1 `shouldBe` True
        it "deplacement" $ do
            prop_envi_inv envi3 `shouldBe` True
            prop_post_deplace idd (C 3 0) envi3 `shouldBe` True 
        it "pre_enleve" $ do
            prop_pre_enleve idd  envi5 `shouldBe` True 
            prop_envi_inv envi5 `shouldBe` True
            
        it "post_enleve" $ do
            prop_post_enleve idd envi6 `shouldBe` True
            prop_envi_inv envi6 `shouldBe` True
            
engineSpec = do
  enviTest
        