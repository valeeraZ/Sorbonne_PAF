module Environnement where

import Lemming 
import Coord 
import Niveau 

import qualified Data.Sequence as S 
import qualified Data.Map as M 
import qualified Data.List as L
import qualified Data.Maybe as Y
import Linear

data Entite = Lem Int Lemming
    deriving Eq 

isLemming :: Entite -> Bool 
isLemming (Lem _ _) = True

isMarcheurDroite :: Entite -> Bool 
isMarcheurDroite (Lem _ (Marcheur Droite _)) = True
isMarcheurDroite (Lem _ (Tombeur Droite _ _)) = True
isMarcheurDroite (Lem _ _) = False

isMarcheurGauche :: Entite -> Bool 
isMarcheurGauche (Lem _ (Marcheur Gauche _)) = True
isMarcheurGauche (Lem _ (Tombeur Gauche _ _)) = True
isMarcheurGauche (Lem _ _) = False

isCreuseur :: Entite -> Bool 
isCreuseur (Lem _ (Creuseur Droite _)) = True
isCreuseur (Lem _ _) = False

isPoseurDroite :: Entite -> Bool 
isPoseurDroite (Lem _ (Poseur Droite _ _)) = True
isPoseurDroite (Lem _ _) = False

isPoseurGauche :: Entite -> Bool 
isPoseurGauche (Lem _ (Poseur Gauche _ _)) = True
isPoseurGauche (Lem _ _) = False

isStoppeur :: Entite -> Bool
isStoppeur (Lem _ (Stoppeur _ _)) = True
isStoppeur (Lem _ _) = False

data Envi = Envi {hEnvi :: Int, lEnvi :: Int, entitesEnvi:: S.Seq Entite,
                    casesEnvi :: M.Map Coord (S.Seq Entite)}
                    deriving Eq 

instance Show Entite where
    show (Lem _ l) = show l 

instance Placable Entite where
    coordP (Lem _ l) = coordP l 
    bougeP d (Lem i l) = Lem i $ bougeP d l
    deplaceP c (Lem i l) =  Lem i $ deplaceP  c l

idEnt :: Entite -> Int
idEnt (Lem i _) = i 

-- |créer un environnement vide
envide :: Int -> Int -> Envi
envide h l = Envi h l S.empty M.empty 

-- |retourner la séquence d'entités d'un environnement
entitesEnvi2 :: Envi -> S.Seq Entite
entitesEnvi2 (Envi h l _ cases) = M.foldl' etape S.Empty cases
    where etape acc s = s <> acc 

-- |chercher une entité par son identifiant dans un environnement
trouveIdEnv :: Int -> Envi -> Maybe Entite
trouveIdEnv n = trouveIdSeq n . entitesEnvi

-- |chercher une entité par son identifiant dans un environnement
trouveIdSeq :: Int -> S.Seq Entite -> Maybe Entite
trouveIdSeq n = foldr etape Nothing
    where etape e acc = if idEnt e == n then Just e else acc 

-- |chercher la coordonnée d'une entité par son identifiant
trouveIdMap :: Int -> M.Map Coord (S.Seq Entite) -> Maybe Coord
trouveIdMap n = M.foldrWithKey etape Nothing
    where etape c s acc =  case trouveIdSeq n s of 
                                Nothing -> acc
                                Just _ -> Just c 

prop_enviInclusion1 :: Envi -> Bool
prop_enviInclusion1 (Envi _ _ ents cases) = foldr etape True ents
    where etape e acc = case trouveIdMap (idEnt e) cases of
                            Nothing -> False
                            Just c -> c == coordP e

prop_enviInclusion2 :: Envi -> Bool
prop_enviInclusion2 (Envi _ _ ents cases) = M.foldrWithKey etape True cases
    where etape c s acc = foldr (etape2 c) acc s
          etape2 c e acc = case trouveIdSeq (idEnt e) ents of
                                Nothing -> False 
                                Just e2 -> acc && coordP e2 == c && coordP e2 == coordP e

prop_envi_inv :: Envi -> Bool
prop_envi_inv envi = prop_enviInclusion1 envi  && prop_enviInclusion2 envi     

instance Show Envi where
    show = showEnvi

showEnvi :: Envi -> String
showEnvi (Envi h l _ cases) = let s = aux 0 (h-1) in s
        where aux x y = if x == (l-1)
                        then (if y == 0 then lacase x y else lacase x y ++ "\n" ++ aux 0 (y+1))
                        else lacase x y ++ aux (x+1) y
              lacase x y = case M.lookup (C x y) cases of
                            Nothing -> " "
                            Just S.Empty -> " "
                            Just (e S.:<| es) -> show e

-- | ??? déterminer une case est vide ou pas par sa coordonnée
caseVide :: Coord -> Envi -> Bool
caseVide (C x y) (Envi h l _ cases) = (x < l) && (x > 0) && (y < h) && (y > 0)

-- |évaluer une fonction(Entite -> Entite) à une entité par son identifiant
appliqueIdSeq :: Int -> (Entite -> Entite) -> S.Seq Entite -> S.Seq Entite
appliqueIdSeq i f = foldr etape S.empty
    where etape n acc
            | idEnt n == i = (f n) S.:<| acc
            | otherwise    = n S.:<| acc

-- |évaluer une fonction(Entite -> Entite) à une entité par son identifiant dans un environnement
appliqueIdEnv :: Int -> (Entite -> Entite) -> Envi -> Envi
appliqueIdEnv n f (Envi h l ents cases) = case trouveIdSeq n ents of
                                            Nothing  -> error $ "appliqueIdEnv : pas trouve l'entite" ++ show n ++ "dans la sequence"
                                            Just e -> Envi h l nents ncases
                                            where nents = appliqueIdSeq n f ents
                                                  ncases = case trouveIdMap n cases of
                                                            Nothing -> error $ "appliqueIdEnv : pas trouve l'entite" ++ show n
                                                            Just endroit -> M.foldrWithKey etape M.empty cases
                                                                where etape co s
                                                                        | co == endroit = M.insert co (appliqueIdSeq n f s)
                                                                        | otherwise = M.insert co s  
-- |enlever une entité à partir de son identifiant
enleveId :: Int -> S.Seq Entite -> S.Seq Entite
enleveId i = foldr etape S.empty
        where etape e acc
                | idEnt e == i = acc
                | otherwise    = e S.:<| acc

-- |enlever une entité à partir de son identifiant dans un environnement
enleveEnvi :: Int -> Envi -> Envi
enleveEnvi n (Envi h l ents cases) =  Envi h l nents ncases
                                        where nents = enleveId n ents
                                              ncases = case trouveIdMap n cases of 
                                                Nothing -> cases
                                                Just endroit -> case M.lookup endroit cases of
                                                    Nothing -> undefined
                                                    Just s -> M.insert endroit (enleveId n s) cases
-- il existe de lemming dans envi
prop_pre_enleve :: Int -> Envi -> Bool  
prop_pre_enleve n (Envi h l ents cases) =  case trouveIdSeq n ents of  
                                            Nothing ->False 
                                            Just e-> True

-- verifier lemming est bien supprimé
prop_post_enleve ::  Int -> Envi -> Bool  
prop_post_enleve n (Envi h l ents cases)  =  case trouveIdSeq n ents of  
                                            Nothing ->True
                                            Just e-> False                                                                                      

-- deplacer une entité vers une nouvelle coordonnée dans un environnement par son identifiant
deplaceDansEnvi :: Int -> Coord -> Envi -> Envi
deplaceDansEnvi n dest (Envi h l ents cases) = case trouveIdSeq n ents of 
                                                    Nothing -> error $ "deplaceDansEnvi: pas toruve l'entité" ++ show n ++ "dans la sequence"
                                                    Just e -> Envi h l nents ncases
                                                        where nents = appliqueIdSeq n (deplaceP dest) ents
                                                              ncases = case trouveIdMap n cases of
                                                                        Nothing -> error $ "deplaceDansEnvi : pas trouve l'entité" ++ show n
                                                                        Just source -> let dents = Y.fromMaybe S.empty $ M.lookup dest cases in
                                                                                       let sents = Y.fromMaybe S.empty $ M.lookup source cases in
                                                                                       let ncases = M.insert source (enleveId n sents) cases in
                                                                                       M.insert dest (deplaceP dest e S.:<| dents) ncases

-- propriete du post condition de deplacement                                                                            
prop_post_deplace :: Int -> Coord -> Envi -> Bool 
prop_post_deplace ide (C x y) (Envi h l ents cases) = case trouveIdMap ide cases of
                                                    Nothing ->False
                                                    Just (C x1 y1) -> x1==x && y1 == y

idFrais :: Envi -> Int
idFrais (Envi h l ents cases) = l + foldr etape 0 ents
    where etape ent = max (idEnt ent)

addEntite :: Entite -> Envi -> Envi
addEntite ent (Envi h l ents cases) = Envi h l nents ncases 
                                        where nents = ent S.:<| ents
                                              ncases = M.insert (coordP ent) (ent S.:<| cents) cases
                                                 where cents = Y.fromMaybe S.empty $ M.lookup (coordP ent) cases

                        
-- propriete du post condition de ajouter un lemming                          
prop_post_addEntite :: Int -> Envi -> Bool 
prop_post_addEntite ide envi1 = case trouveIdEnv ide envi1 of
                                Nothing -> False 
                                Just e -> idEnt e == ide

changeMarcheur :: Entite -> Entite
changeMarcheur (Lem i (Marcheur dir c)) = Lem i (Marcheur dir c)
changeMarcheur (Lem i (Creuseur dir c)) = Lem i (Marcheur dir c)
changeMarcheur (Lem i (Stoppeur dir c)) = Lem i (Marcheur dir c)
changeMarcheur (Lem i (Poseur dir _ c)) = Lem i (Marcheur dir c)
changeMarcheur (Lem i (Tombeur dir t c)) = Lem i (Tombeur dir t c)

changeCreuseur :: Entite -> Entite
changeCreuseur (Lem i (Marcheur dir c)) = Lem i (Creuseur dir c)
changeCreuseur (Lem i (Creuseur dir c)) = Lem i (Creuseur dir c)
changeCreuseur (Lem i (Stoppeur dir c)) = Lem i (Creuseur dir c)
changeCreuseur (Lem i (Poseur dir _ c)) = Lem i (Creuseur dir c)
changeCreuseur (Lem i (Tombeur dir t c)) = Lem i (Tombeur dir t c)

changePoseur :: Entite -> Entite
changePoseur (Lem i (Marcheur dir c)) = Lem i (Poseur dir 0 c)
changePoseur (Lem i (Creuseur dir c)) = Lem i (Poseur dir 0 c)
changePoseur (Lem i (Stoppeur dir c)) = Lem i (Poseur dir 0 c)
changePoseur (Lem i (Poseur dir n c)) = Lem i (Poseur dir n c)
changePoseur (Lem i (Tombeur dir t c)) = Lem i (Tombeur dir t c)

changeStoppeur:: Entite -> Entite
changeStoppeur (Lem i (Marcheur dir c)) = Lem i (Stoppeur dir c)
changeStoppeur (Lem i (Creuseur dir c)) = Lem i (Stoppeur dir c)
changeStoppeur (Lem i (Stoppeur dir c)) = Lem i (Stoppeur dir c)
changeStoppeur (Lem i (Poseur dir _ c)) = Lem i (Stoppeur dir c)
changeStoppeur (Lem i (Tombeur dir t c)) = Lem i (Tombeur dir t c)
