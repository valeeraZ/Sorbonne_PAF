module Td1 where

-- Question 1.1

(==>) :: Bool -> Bool -> Bool 
(==>) True False = False 
(==>) _ _ = True
infixr 1 ==>

-- >>> True ==> False
-- False

-- >>> False ==> (True ==> False)
-- True

-- associer à droite
-- >>> False ==> True ==> False
-- True

prop_implTrans :: Bool -> Bool -> Bool -> Bool
prop_implTrans a b c = (a ==> b) && (b ==> c) ==> a ==> c

-- >>> prop_implTrans True True True
-- True

-- On test tous les cas pour cette propriété (8 cas) que celles soient tout le temp vraies

-- Question 1.2

-- Question 1.3

-- Hypothèse : n est un entier naturel
somme :: Integer -> Integer
somme 0 = 0
somme n = n + somme (n-1)

-- >>> somme 11
-- 66

-- Question 1.4

prop_sommeCroiss :: Integer -> Integer -> Bool 
prop_sommeCroiss a b = (a <= b) ==> ( somme a <= somme b )

-- >>> prop_sommeCroiss 10 100
-- True
-- >>> prop_sommeCroiss 100 10
-- True

prop_sommeCorrec :: Integer -> Bool
prop_sommeCorrec n
    | n < 0 = True
    | otherwise = somme n == (n * (n + 1)) `div` 2

-- `div` pour entier, / pour float

-- >>> prop_sommeCorrec 10
-- True

-- Le programmeur écrit la fonction en Haskell
-- puis on cherche les propriétés qu'elle doit vérifier (Invariant, pre-cond, post-cond)
-- Le programmeur écrit les propriétés en Haskell
-- Le programmeur fait des tests, basés sur les propriétés

-- Stratégie: Test vs Preuve

-- Question 2.1

-- Donné
    -- Produit: attributs en A * B * C, les A B C sont de différents types
    -- Somme: attributs en A + B, soit type de A soit type de B

data Vide

exnihilo :: Vide -> Integer 
exnihilo _ = 3

data Cuve = 
    Cuve Integer Integer -- capacité & quantité
    | CuveVide Integer -- capacité
    | CuvePleine Integer -- capacité
    deriving Show

prop_cuveInvariant :: Maybe Cuve -> Bool
prop_cuveInvariant (Just (CuveVide cap)) = cap >= 0
prop_cuveInvariant (Just (CuvePleine cap)) = cap >= 0
prop_cuveInvariant (Just (Cuve cap cour)) = cap > 0 && cour > 0 && cour <= cap

initCuve :: Integer -> Maybe Cuve
initCuve c 
    | c > 0 = Just $ CuveVide c
    | otherwise = Nothing 

-- >>> initCuve 3
-- Just (CuveVide 3)

-- >>> prop_cuveInvariant (initCuve 6)
-- True

quantite :: Cuve -> Integer
quantite (Cuve _ q) = q
quantite (CuveVide c) = 0
quantite (CuvePleine c) = c

capacite :: Cuve -> Integer
capacite (Cuve c _) = c
capacite (CuveVide c) = c
capacite (CuvePleine c) = c

-- changer l'état d'une cuve (pas nouvelle cuve)

changeCuve :: Cuve -> Integer -> Cuve
changeCuve cu q
    | q == 0 = CuveVide (capacite cu) 
    | q == capacite cu = CuvePleine (capacite cu)
    | otherwise  = Cuve (capacite cu) q

-- v: volume
remplirCuve :: Cuve -> Integer -> Maybe Cuve
remplirCuve _ v | v <= 0 = Nothing 
remplirCuve cu v = 
    let q = v + quantite cu in 
        if q <= capacite cu
            then Just $ changeCuve cu q
            else Nothing

-- v: volume
viderCuve :: Cuve -> Integer -> Maybe Cuve
viderCuve _ v | v <= 0 = Nothing 
viderCuve cu v = 
    let q = quantite cu - v in 
        if q >= 0
            then Just $ changeCuve cu q
            else Nothing



prop_viderPost :: Cuve -> Integer -> Bool
prop_viderPost cu v = case viderCuve cu v of
                        Nothing  -> True
                        Just cu2 -> quantite cu2 + v == quantite cu



-- >>> prop_viderPost (Cuve 30 10) 10
-- True


data Tuyau = Tuyau {
entree :: Cuve
, sortie :: Cuve
, porteEnt :: Bool
, porteSor :: Bool
, contenu :: Cuve
} deriving (Show)

prop_tuyauInv1 :: Tuyau -> Bool
prop_tuyauInv1 tu = prop_cuveInvariant (Just (entree tu))
                    && prop_cuveInvariant (Just (sortie tu))
                    && prop_cuveInvariant (Just (contenu tu))

prop_tuyauInv2 :: Tuyau -> Bool
prop_tuyauInv2 tu = porteEnt tu ==> not (porteSor tu)

initTuyau :: Cuve -> Cuve -> Integer -> Maybe Tuyau
initTuyau cuIn cuOut cap =
    case initCuve cap of
        Nothing -> Nothing
        Just cu -> Just $ Tuyau cuIn cuOut False False cu

-- Question 2.4

data DoorSide = IN | OUT
    deriving (Show)

switchDoor :: Tuyau -> DoorSide -> Tuyau
-- la porte d'entrée est ouverte, on souhaite la fermer
switchDoor tu@Tuyau {porteEnt = True} IN = tu {porteEnt = False }
-- la porte d'entrée est fermée, on souhaite la ouvrir
switchDoor tu@Tuyau {entree = cui, porteEnt = False, contenu = cu} IN = 
    let place = capacite cu - quantite cu
        volume = min place (quantite cui)
    in case (remplirCuve cu volume, viderCuve cui volume) of
        (Just cu2, Just cui2) -> tu {entree = cu2, porteEnt = True  }
        (_,_) -> error "bug"
