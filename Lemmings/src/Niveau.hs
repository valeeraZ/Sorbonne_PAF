module Niveau where

import qualified Data.List as L 
import qualified Data.Map.Strict as M
import Coord 
import Linear (V4(..))
import System.IO
import System.Random


data Case = Metal | Terre | Entree | Sortie | Vide 
    deriving Eq 

instance Show Case  where 
    show Vide = " "
    show Metal = "X"
    show Terre = "0"
    show Entree = "E"
    show Sortie = "S"

instance Read Case where
    readsPrec _ x = [(chaineDeCase x,"")]

chaineDeCase :: String -> Case
chaineDeCase " " = Vide 
chaineDeCase "X" = Metal 
chaineDeCase "0" = Terre 
chaineDeCase "E" = Entree
chaineDeCase "S" = Sortie 
chaineDeCase _ = Vide 

data Niveau = Niveau {hniveau :: Int, lNiveau :: Int, casesNiveau :: M.Map Coord Case} 
    deriving Eq 

instance Show Niveau where
    show (Niveau h l cases) = let (s,_,_) = M.foldl' aux ("",0,0) cases in s 
        where aux (s,x,y) v = if x == (l - 1) 
                            then (if y == (h - 1) then (s ++ show v,0,0) else (s ++ show v ++ "\n", 0, y+1))
                            else (s ++ show v , x+1, y)

instance Read Niveau where
    readsPrec _ x = [(retourneNiveau (chaineDeNiveau x), "")]

chaineDeNiveau :: String -> Niveau
chaineDeNiveau = (\(cases,l,h) -> Niveau (h+1) l cases) . L.foldl' aux (M.empty ,0,0) where 
     aux (cases,x,y) '\n' = (cases,0,y+1)
     aux (cases,x,y) c = (M.insert (C x y) (read [c]) cases, x+1, y)

retourneNiveau :: Niveau -> Niveau
retourneNiveau (Niveau h l cases) = Niveau h l $ M.foldrWithKey etape M.empty cases
    where etape (C x y) c = M.insert (C x y) c 

-- |lire des niveaux depuis un fichier text
niveauDeFichier :: FilePath -> IO (Niveau)
niveauDeFichier fp = do
    file <- readFile fp
    return (read file)

-- verifier le niveau est bien fermé
prop_niveauFerme :: Niveau -> Bool
prop_niveauFerme (Niveau h l cases) = M.foldrWithKey etape True cases 
    where etape (C x y) c acc
            | x == 0 || x == l - 1 || y == 0 || y == h - 1 = (c == Metal) && acc
            | otherwise = acc

--verifier il exist un entree et une sortie
prop_niveauEntreeSortie :: Niveau -> Bool
prop_niveauEntreeSortie (Niveau h l cases) = let (e,s) = M.foldrWithKey etape (0,0) cases in e == 1 && s == 1
    where etape (C x y) Entree (en,so) = (en+1,so)
          etape (C x y) Sortie (en,so) = (en,so+1)
          etape (C x y) _ acc = acc 

trouveEntree :: Niveau -> Maybe Coord 
trouveEntree (Niveau h l cases) = M.foldrWithKey  etape Nothing cases 
    where etape c Entree _ = Just c 
          etape _ _ acc = acc 

-- |chercher si il existe la sortie sur le niveau et renvoyer sa coordonnée
trouveSortie :: Niveau -> Maybe Coord 
trouveSortie (Niveau h l cases) = M.foldrWithKey  etape Nothing cases 
    where etape c Sortie _ = Just c 
          etape _ _ acc = acc  

--verifier l'entree est au desous d'un case vide
prop_niveauEntreeCorrecte :: Niveau -> Bool 
prop_niveauEntreeCorrecte niv = case trouveEntree niv of
                                     Nothing -> False
                                     Just (C x y) -> case M.lookup (C x (y-1)) (casesNiveau niv) of 
                                                        Just Vide -> True
                                                        _ -> False 

-- verifier la sortie est au desous d'un case metal
prop_niveauSortieCorrecte :: Niveau -> Bool 
prop_niveauSortieCorrecte niv = case trouveSortie niv of
                                     Nothing -> False
                                     Just (C x y) -> case M.lookup (C x (y-1)) (casesNiveau niv) of 
                                                        Just Metal -> True
                                                        _ -> False                                                      

prop_niveauInclusion :: Niveau -> Bool
prop_niveauInclusion (Niveau h l cases) = M.foldrWithKey etape True cases
    where etape (C x y) _ acc = acc && (x >= 0) && (x < l) && (y >= 0) && (y < h)

prop_niveauInvariant :: Niveau -> Bool
prop_niveauInvariant niv = prop_niveauFerme niv && prop_niveauEntreeSortie niv && prop_niveauEntreeCorrecte niv  && prop_niveauSortieCorrecte niv &&  prop_niveauInclusion niv 

passable :: Coord -> Niveau -> Bool
passable c (Niveau h l cases) = case M.lookup c cases of
                                    Just Vide -> True
                                    Just Entree -> True
                                    Just Sortie -> True
                                    _ -> False

dur :: Coord -> Niveau -> Bool
dur c (Niveau h l cases) = case M.lookup c cases of
                                Just Metal -> True
                                Just Terre -> True
                                _ -> False 

-- Verifier si la case en bas peut être creusé.
creusable :: Coord -> Niveau -> Bool
creusable c (Niveau h l cases) = case M.lookup c cases of
                                Just Terre -> True
                                _ -> False

-- creuser une case positionnée à Coord dans le niveau donné (attention pre condition)
creuser :: Coord -> Niveau -> Niveau
creuser c (Niveau h l cases) = Niveau h l (M.update (\_ -> Just Vide) c cases)

-- stopper, faire un mur (increusable) sur la case positionnée à Coord dans le niveau donné
stopper :: Coord -> Niveau -> Niveau
stopper c (Niveau h l cases) = Niveau h l (M.update (\_ -> Just Metal) c cases)

-- retirer le mur fait par stopper (pre & post conditon)
destopper :: Coord -> Niveau -> Niveau
destopper c (Niveau h l cases) = Niveau h l (M.update (\_ -> Just Vide) c cases)

-- poser un escalier sur une case positionné à Coord dans le niveau donné
poser :: Coord -> Niveau -> Niveau
poser c (Niveau h l cases) = Niveau h l (M.update (\_ -> Just Terre) c cases)


caseSuivante :: (Int -> Int -> Int -> [(Coord,Case)]) -> Int -> Int -> Int -> [(Coord,Case)]
caseSuivante aux i j c = 
    if i == 9
        then 
            if j == 12
                then []
            else  aux 0 (j+1) c
    else aux (i+1) j c

niveauGenerator :: Int ->  Niveau
niveauGenerator c = 
    let larg = 10 in
    let haut = 13 in 
    Niveau haut larg (M.fromList (aux 0 0 c)) 
    where
        aux :: Int -> Int -> Int -> [(Coord,Case)]
        --creer les mur
        aux 0 j c = let nouv_case = Metal in           
                [(C 0 j,nouv_case)] <> caseSuivante aux 0 j c
        aux 9 j c = let nouv_case = Metal in
                [(C 9 j,nouv_case)] <> caseSuivante aux 9 j c
        aux i 0 c = let nouv_case = Metal in
                [(C i 0,nouv_case)] <> caseSuivante aux i 0 c
        aux i 12 c = let nouv_case = Metal in                   
                [(C i 12,nouv_case)] <> caseSuivante aux i 12 c
        -- creer entre et sortie
        aux i j c = let a = mod c 8 + 1 in              
                    let d = mod c 2 + 3 in              
                    let f = mod c 7 + 2 in
                    if j == d || j == f                   
                    then if i == a && j == d
                        then let nouv_case = Vide in 
                        [(C i j,nouv_case)] <> caseSuivante aux i j c 
                        else 
                            if i == (mod (a+7) 8 ) + 1 && j == f
                            then let nouv_case = Terre in 
                             [(C i j,nouv_case)] <> caseSuivante aux i j c
                            else let nouv_case = Metal in 
                            [(C i j,nouv_case)] <> caseSuivante aux i j c 
                    else if ((i == mod (a+4) 9  + 1) && j>f && j< d ) || ((i == mod (a+9) 9 + 1) && j > 0 && j < max 9 f)    -- Si il s'agit d'un mur vertical
                        then 
                            if j == 10 || j == (f-2)
                            then let nouv_case = Vide in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c
                            else let nouv_case = Terre in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c                       
                        else  if j == 11 && i == mod (a+8) 9 + 1 
                            then let nouv_case = Entree in 
                            [(C i j,nouv_case)] <> caseSuivante aux i j c
                            else if j == 1 && i == a
                                then let nouv_case = Sortie in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c
                                else let nouv_case = Vide in 
                                [(C i j,nouv_case)] <> caseSuivante aux i j c 

generateNiveauVerified :: StdGen -> Niveau
generateNiveauVerified g =
    let c = randomRs (1, 20) g  in
    let niv = niveauGenerator (head c) in 
    aux 0 niv 
    where 
        aux :: Int -> Niveau -> Niveau
        aux 12_ = error "Cela fait 12 tours que le niveau n'est pas saine" 
        aux i niveau = 
            if prop_niveauInvariant niveau 
            then niveau
            else let c = randomRs (1, 20) g  in
                aux (i+1) (niveauGenerator (head c))        
