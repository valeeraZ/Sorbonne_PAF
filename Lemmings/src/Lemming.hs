module Lemming where

import Coord

data Direction = Gauche | Droite
    deriving (Eq,Show)

data Lemming = Marcheur Direction Coord
             | Tombeur Direction Int Coord
             | Creuseur Direction Coord
             | Stoppeur Direction Coord
             | Poseur Direction Int Coord
             | Mort Coord
        deriving Eq 


instance Show Lemming where
    show (Mort _) = "+"
    show (Marcheur Gauche _) = "<"
    show (Marcheur Droite _) = ">"
    show (Tombeur _ _ _) = "v"
    show (Creuseur _ _) = "c"
    show (Poseur _ _ _) = "p"
    show (Stoppeur _ _) = "s"

instance Placable Lemming where
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming

coordLemming :: Lemming -> Coord
coordLemming (Marcheur _ c) = c
coordLemming (Mort c) = c 
coordLemming (Tombeur _ _ c) = c
coordLemming (Creuseur _ c) = c
coordLemming (Stoppeur _ c) = c
coordLemming (Poseur _ _ c) = c 

-- |tuer un lemming 
tueLemming :: Lemming -> Lemming
tueLemming l = Mort (coordLemming l)

prop_tueLemming_post :: Lemming -> Bool
prop_tueLemming_post l = case tueLemming l of 
                            Mort c -> c == coordLemming l
                            _ -> False 

bougeLemming :: Deplacement -> Lemming -> Lemming
bougeLemming dep (Marcheur dir c ) = Marcheur dir (bougeCoord dep c)
bougeLemming dep (Mort c) = Mort (bougeCoord dep c)
bougeLemming dep (Tombeur dir n c ) = Tombeur dir n (bougeCoord dep c)
bougeLemming dep (Creuseur dir c) = Creuseur dir (bougeCoord dep c)
bougeLemming dep (Poseur dir n c) = Poseur dir n (bougeCoord dep c)
bougeLemming dep (Stoppeur dir c) = Stoppeur dir (bougeCoord dep c)

prop_bougeLemmingDHDBG :: Lemming -> Bool
prop_bougeLemmingDHDBG l = bougeLemming D l == (bougeLemming G . bougeLemming DB . bougeLemming DH) l

deplaceLemming :: Coord -> Lemming -> Lemming
deplaceLemming c (Mort _) = Mort c
deplaceLemming c (Marcheur d _) = Marcheur d c
deplaceLemming c (Tombeur d n _) = Tombeur d n c
deplaceLemming c (Creuseur d _) = Creuseur d c 
deplaceLemming c (Poseur d n _) = Poseur d n c
deplaceLemming c (Stoppeur d _) = Stoppeur d c
