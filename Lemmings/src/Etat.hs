{-# LANGUAGE BlockArguments #-}
module Etat where

import Lemming 
import Coord 

import Environnement

import Niveau
import Linear
import qualified Keyboard as K

import qualified SDL as SD
import qualified Data.Sequence as S
import Mouse (Mouse)
import Control.Monad.State ( MonadState(put), MonadState(get), evalState, execState, modify, State )


data Etat = Etat {
    enviE :: Envi,
    niveauE :: Niveau,
    lrestantsE :: Int,
    lvivantsE :: Int,
    lsauvesE :: Int,
    lemmingSelect :: Int 
}

modifyEnvi :: Envi -> State Etat ()
modifyEnvi envi = do
    etat@(Etat _ niv r v s l) <- get 
    put (Etat envi niv r v s l)

modifyNiveau :: Niveau -> State Etat ()
modifyNiveau niv = do 
    etat@(Etat envi _ r v s l) <- get 
    put (Etat envi niv r v s l)

lemmingSortir :: Int -> State Etat ()
lemmingSortir n = do
    etat@(Etat envi niv r v s l) <- get
    put (Etat envi niv r (v-1) (s+1) l)
    modifyEnvi (enleveEnvi n envi)

lemmingMourir :: Int -> State Etat ()
lemmingMourir n = do
    etat@(Etat envi niv r v s l) <- get
    put (Etat envi niv r (v-1) s l)
    modifyEnvi (enleveEnvi n envi)

popLemState :: State Etat ()
popLemState = do
    etat@(Etat envi niv r v s l) <- get 
    case trouveEntree niv of
        Nothing -> put (Etat envi niv r v s l)
        Just c -> put (Etat nenvi niv (r-1) (v+1) s l)
                where nenvi = addEntite nlem envi
                      nlem = Lem (idFrais envi) (Tombeur Droite 0 c)
    
data Fin = Victoire Int | Defaite

selectLemming :: Int -> Etat -> Etat
selectLemming new_id (Etat envi niv r v s _) = Etat envi niv r v s new_id

prop_etat::Etat -> Bool 
prop_etat (Etat envi niv r v s _) = (r >= 0)&&(v >= 0)&&(s >=0)

-- |definir la hauteur où le lemming peut tomber mort
hauteurMortelle :: Int
hauteurMortelle = 5

-- |rassembler l'environnement avec le niveau
rassembleEnvNiv :: String -> String -> String
rassembleEnvNiv [] _ = [] 
rassembleEnvNiv _ [] = []
rassembleEnvNiv (x1:xs1) (x2:xs2) = if x1 == ' ' then x2:rassembleEnvNiv xs1 xs2 else x1:rassembleEnvNiv xs1 xs2 

showEtat :: Etat -> String
showEtat e = rassembleEnvNiv (show (enviE e)) (show (niveauE e))

instance Show Etat where
    show = showEtat

-- |retourner un nouvel état d'après l'état ancien pour un lemming par son identifiant après un tour
tourLemming :: Int -> Lemming -> State Etat ()
tourLemming n (Mort c) = lemmingMourir n 

tourLemming n (Marcheur Gauche c) = do
    etat@(Etat envi niv r v s l) <- get 
    case trouveSortie niv of
        Nothing -> apres niv envi
        Just coord -> if coord == c then lemmingSortir n else apres niv envi
        where apres niv envi =
                case (passable (gauche c) niv && passable (haut (gauche c)) niv,
                        passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, 
                        dur (bas c) niv) of
                (_, _, False) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Tombeur Gauche 0 c)) envi)
                (True, _, _) -> modifyEnvi (deplaceDansEnvi n (gauche c) envi)
                (_, True, _) -> modifyEnvi (deplaceDansEnvi n (haut (gauche c)) envi)
                (_, _, _) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Marcheur Droite c)) envi)
                    
tourLemming n (Marcheur Droite c) = do
    etat@(Etat envi niv r v s l) <- get
    case trouveSortie niv of
        Nothing -> apres niv envi
        Just coord -> if coord == c then lemmingSortir n else apres niv envi
        where apres niv envi = 
                case (passable (droite c) niv && passable (haut (droite c)) niv,
                     passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, 
                     dur (bas c) niv) of
                (_, _, False) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Tombeur Droite 0 c)) envi)
                (True, _, _) -> modifyEnvi (deplaceDansEnvi n (droite c) envi) 
                (_, True, _) -> modifyEnvi (deplaceDansEnvi n (haut (droite c)) envi)
                (_, _, _) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Marcheur Gauche c)) envi)

tourLemming n (Tombeur dir k c) = do
    etat@(Etat envi niv r v s l) <- get
    case (dur (bas c) niv, k >= hauteurMortelle) of
        (True, True) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Mort c)) envi)
        (True, _) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Marcheur dir c)) envi)
        (_, _) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Tombeur dir (k+1) (bas c))) (deplaceDansEnvi n (bas c) envi))

tourLemming n (Creuseur dir c) = do
    etat@(Etat envi niv r v s l) <- get
    case trouveSortie niv of
        Nothing -> apres niv envi
        Just coord -> if coord == c then lemmingSortir n else apres niv envi
        where apres niv envi = 
                case (dur (bas c) niv, creusable (bas c) niv, creusable (bas (gauche c)) niv, creusable (bas (droite c)) niv) of
                -- la case en bas n'est pas dur, tomber
                (False, _, _, _) -> modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Tombeur dir 0 c)) envi)
                -- seule la case en bas est creusable
                (True, True, False, False) -> do
                                                modifyEnvi (deplaceDansEnvi n (bas c) envi) 
                                                modifyNiveau (creuser (bas c) niv)
                -- la case en bas et celle en bas à gauche sont creusables
                (True, True, True, False) -> do
                                                modifyEnvi (deplaceDansEnvi n (bas c) envi) 
                                                modifyNiveau (creuser (bas c) (creuser (bas (gauche c)) niv))
                -- la case en bas, celle en bas à gauche et celle en bas à droite sont creusables
                (True, True, True, True) -> do
                                                modifyEnvi (deplaceDansEnvi n (bas c) envi) 
                                                modifyNiveau (creuser (bas c) (creuser (bas (droite c)) (creuser (bas (gauche c)) niv)))
                -- aucune case n'est creusable, le lemming devient un marcheur
                (_, _, _, _) -> tourLemming n (Marcheur dir c)

-- t <= 4: 4 marches
tourLemming n (Poseur Gauche t c) = do
    etat@(Etat envi niv r v s l) <- get
    case trouveSortie niv of
        Nothing -> apres niv envi
        Just coord -> if coord == c then lemmingSortir n else apres niv envi
        where apres niv envi = 
                if t <= 4 then
                    case (passable (haut (gauche c)) niv && passable (haut (haut (gauche c))) niv, passable (gauche c) niv) of
                    (True, False) -> do 
                                        modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Poseur Gauche (t+1) c)) (deplaceDansEnvi n (haut (gauche c)) envi)) 
                                        modifyNiveau (poser (gauche c) niv)
                    (True, True) -> do
                                        modifyEnvi (deplaceDansEnvi n (haut (gauche c)) envi) 
                                        modifyNiveau (poser (gauche c) niv)
                    (_, _) -> do
                                modifyEnvi (appliqueIdEnv n (\_ -> Lem n (Marcheur Gauche c)) envi)
                else
                    tourLemming n (Marcheur Gauche c)

tourLemming n (Poseur Droite t c) = do
    etat@(Etat envi niv r v s l) <- get
    case trouveSortie niv of
        Nothing -> apres niv envi
        Just coord -> if coord == c then lemmingSortir n else apres niv envi
        where apres niv envi = 
                if t <= 4 then
                    case (passable (haut (droite c)) niv && passable (haut (haut (droite c))) niv, passable (droite c) niv) of
                    (True, False) -> do 
                                        modifyEnvi (appliqueIdEnv n (\ _ -> Lem n (Poseur Droite (t+1) c)) (deplaceDansEnvi n (haut (droite c)) envi)) 
                                        modifyNiveau (poser (droite c) niv)
                    (True, True) -> do 
                                        modifyEnvi (deplaceDansEnvi n (haut (droite c)) envi) 
                                        modifyNiveau (poser (droite c) niv)
                    (_, _) -> do
                                modifyEnvi (appliqueIdEnv n (\_ -> Lem n (Marcheur Droite c)) envi)
                else
                    tourLemming n (Marcheur Droite c)

-- non deplacement, 
tourLemming n stoppeur@(Stoppeur dir c) = do
    etat@(Etat envi niv r v s l) <- get
    modifyEnvi (appliqueIdEnv n (\_ -> Lem n stoppeur) envi) 
    modifyNiveau (stopper c niv)


-- |retourner un nouvel état pour un changement d'entité dans l'état ancien
tourEntite :: Int -> Etat -> Etat
tourEntite n et = case trouveIdEnv n (enviE et) of
                    Nothing -> et
                    Just (Lem _ l) -> execState (tourLemming n l) et

-- |ajouter une nouvelle lemming à l'état
popLem :: Etat -> Etat
popLem (Etat envi niv r v s l) = case trouveEntree niv of
                                Nothing -> Etat envi niv r v s l
                                Just c -> Etat nenvi niv (r-1) (v+1) s l
                                            where nenvi = addEntite nlem envi
                                                  nlem = Lem (idFrais envi) (Tombeur Droite 0 c)
-- |change le type d'une entité(Lemming), puis effacer l'entité(Lemming) selectionné avant (lemmingSelected - Nothing)
changeC :: Etat -> Etat
changeC etat@(Etat envi niv r v s l) = do
        case trouveIdEnv l envi of
            Just entite -> 
                let new_niv = destopper (coordP entite) niv in
                    Etat (appliqueIdEnv l changeCreuseur envi) new_niv r v s 0
            Nothing -> etat

changeP :: Etat -> Etat
changeP etat@(Etat envi niv r v s l) = do
        case trouveIdEnv l envi of
            Just entite -> 
                let new_niv = destopper (coordP entite) niv in
                    Etat (appliqueIdEnv l changePoseur envi) new_niv r v s 0
            Nothing -> etat

changeS :: Etat -> Etat
changeS etat@(Etat envi niv r v s l) = do
        case trouveIdEnv l envi of
            Just entite -> 
                let new_niv = destopper (coordP entite) niv in
                    Etat (appliqueIdEnv l changeStoppeur envi) new_niv r v s 0
            Nothing -> etat

changeM :: Etat -> Etat
changeM etat@(Etat envi niv r v s l) = do
        case trouveIdEnv l envi of
            Just entite -> 
                let new_niv = destopper (coordP entite) niv in
                    Etat (appliqueIdEnv l changeMarcheur envi) new_niv r v s 0
            Nothing -> etat

keyChangeLemming :: K.Keyboard -> State Etat ()
keyChangeLemming kbd = do
    etat <- get
    let action | K.keypressed SD.KeycodeC kbd = modify changeC
               | K.keypressed SD.KeycodeP kbd = modify changeP
               | K.keypressed SD.KeycodeS kbd = modify changeS
               | K.keypressed SD.KeycodeM kbd = modify changeM
               | otherwise = put etat
    action

changeLemming :: K.Keyboard -> State Etat ()
changeLemming kbd = do
    etat <- get
    if lemmingSelect etat > 0 
        then keyChangeLemming kbd
        else put etat

inZone :: Entite -> Coord -> Bool
inZone entite (C x1 y1) = let (C x0 y0) = coordP entite in 
                              let x = (x0*50) in 
                                  let y = (y0*50) in
                                        x1 <= (x+50) && y1 <= (y+50) && x1 >= x && y1 >= y


trouveEntiteSelected :: Coord -> S.Seq Entite -> Maybe Entite
trouveEntiteSelected n ents = S.lookup 0 $ S.filter (`inZone` n) ents

trouveIdEntiteSelected ::  Mouse -> S.Seq Entite -> Maybe Int
trouveIdEntiteSelected mouse map = case mouse of 
       Nothing -> Nothing
       Just (V2 x1 y1) ->  case trouveEntiteSelected (C x1 y1) map of
                                Nothing -> Nothing
                                Just (Lem i l) -> Just i 

-- |retourner un nouvel état ou une victoire après un tour
tourEtat :: Int -> Etat -> Maybe (V2 Int) -> K.Keyboard -> Either Fin Etat
tourEtat t etat coordinates kbd = 
    let e = execState (changeLemming kbd) etat in
        let selectedLemming = trouveIdEntiteSelected coordinates (entitesEnvi2 (enviE e)) in
            case selectedLemming of
                Nothing -> 
                    (verif . pop) $ foldr etape e (entitesEnvi2 (enviE e))
                Just id -> do
                    let e1 = selectLemming id e in  
                        (verif . pop) $ foldr etape e1 (entitesEnvi2 (enviE e1))
                where etape enti acc = tourEntite (idEnt enti) acc
                      pop = if restants > 0 && t `mod` 5 == 0 then popLem else id 
                      restants = lrestantsE etat
                      verif et 
                        | lrestantsE et == 0 && lvivantsE et == 0 && lsauvesE et >= 5 = Left $ Victoire $ lsauvesE et
                        | lrestantsE et == 0 && lvivantsE et == 0 && lsauvesE et < 5 = Left Defaite 
                        | otherwise =  Right et          
