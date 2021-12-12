
{-# LANGUAGE OverloadedStrings #-}

module Cours3 where

-- on va utiliser le text
import Data.Text (Text)
import qualified Data.Text as Text

-- imports n�cessires pour les s�quences
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq   -- ou S ou le pr�fixe que vous voulez, Seq dans le cours PAF

-- utilitaires de tables associatives
import Data.Map (Map)
import qualified Data.Map as Map

-- utilitaires d'ensembles
import Data.Set (Set)
import qualified Data.Set as Set

{-

# Th�me 1 - TyDD - Cours 3 : Architecture "PAF" + types conteneurs

Cours en deux parties :

1) petit r�cap. m�thodologique et technique

2) les structures discr�tes  : s�quences, tables associatives (map) et ensembles (set)

-}


{-

## Petit r�cap. m�thodologique

On commence donc par un r�capitulatif sur la mod�lisation dirig�e par les types
et l'architecture des projets PAF/Haskell

L 'architecture g�n�rale des projets d�velopp�s en Haskell dans le contexte du cours
PAF est (l�g�rement) inspir�e du principe du
Functional Domain Modelling et (plus fortement) de ce que l'on
appelle les m�thodes formelles de G.L. (m�thodes Z, B, VDM, etc.)

(cf. figure)

-}

{-

### Principe 1 : les entit�s (du domain) sont des sommes-de-produits (records)

Et on distingue les entit�s d'un c�t� et les valeurs de programmation sans domaine sp�cifique
(pour impl�menter des algorithmes)...

-}


data Tank =
  Tank Integer Integer
  | FullTank Integer
  | EmptyTank Integer
  deriving (Show)

capacity :: Tank -> Integer
capacity (Tank _ cap) = cap
capacity (FullTank cap) = cap
capacity (EmptyTank cap) = cap

quantity :: Tank -> Integer
quantity (Tank qty _) = qty
quantity (FullTank cap) = cap
quantity (EmptyTank cap) = 0

{-

### Principe 2 : les �tats remarquables ont des constructeurs sp�cifiques
(voir des types sp�cifiques)

Exemple pour la cuve :  le fait d'�tre vide, pleine, ou remplie partiellement

-}

{-

### Principe 3 : les �tats incoh�rents ne sont pas constructibles

Exemple d'�tat incoh�rent : une cuve avec la quantit� sup�rieure � la capacit�

-}

mauvaiseCuve :: Tank
mauvaiseCuve = Tank 20 10   -- 20L dans une cuve de 10L ?????

-- la propri�t� suivante est un invariant sur les cuves
prop_cuveCorrect :: Tank -> Bool
prop_cuveCorrect (Tank qty cap) = 0 <= qty && qty < cap
prop_cuveCorrect (EmptyTank cap) = cap > 0
prop_cuveCorrect (FullTank cap) = cap > 0

prop_tank_inv = prop_cuveCorrect

-- >>> prop_cuveCorrect mauvaiseCuve
-- False

-- Suggestion : faire un constructeur qui v�rifie ==> smart constructeur
mkTank :: Integer -> Maybe Tank    -- encore mieux : avec Either et des cas d'erreurs
mkTank cap | cap > 0 = Just $ EmptyTank cap
           | otherwise = Nothing

-- � prouver : forall cap :: Integer,  cap > 0 ==> prop_cuveCorrect (fromJust (mkTank cap))

{-

### Principe 4 : d�finition de propri�t�s sur les entit�s : les invariants
et sur les fonctions

Sur les entit�s, ce sont des invariants
Sur les fonctions, on va distinguer les pr�conditions et les postconditions

Pr�condition : propri�t� assum�e vraie avant d'appeler la fonction
Postcondition : propri�t� esp�r�e variant apr�s l'appel de la fonction
                (exemple : la pr�servation de l'invariant)

-}

-- initialisation
initTank_pre :: Integer -> Bool
initTank_pre cap = cap > 0

initTank :: Integer -> Maybe Tank
initTank cap = undefined

prop_initTank_inv :: Integer -> Bool
prop_initTank_inv cap =
  initTank_pre cap
  && case initTank cap of
       Just tank -> prop_tank_inv tank
       Nothing -> False -- initialisation should work

-- transition
fillTank_pre :: Tank -> Integer -> Bool
fillTank_pre tank vol = ((quantity tank) + vol) <= (capacity tank)   

fillTank :: Tank -> Integer -> Maybe Tank
fillTank tank vol = undefined

prop_fillTank_inv :: Tank -> Integer -> Bool
prop_fillTank_inv beforeTank vol =
  prop_tank_inv beforeTank
  && fillTank_pre beforeTank vol
  && case fillTank beforeTank vol of
       Just afterTank -> prop_tank_inv afterTank
       Nothing -> False -- on n'a pas le droit � un non-�v�nement... 

{-{

**Remarque** : la traduction des propri�t�s pour qu'elles soient exploitables
dans quickcheck n'est pas forc�ment trivial, mais on a d�j� l� quelque chose
d'int�ressant � tester de fa�on classique (cf. Hspec).

}-}

-- exemple de postcondition suppl�mentaire (uniquement int�ressante avant d'impl�menter)

fillTank_positive_post :: Tank -> Integer -> Tank -> Bool
fillTank_positive_post beforeTank vol afterTank =
  prop_tank_inv beforeTank
  && fillTank_pre beforeTank vol
  && prop_tank_inv afterTank
  && quantity afterTank == quantity beforeTank + vol

{-

### Principe 5 : d�couvrir et formaliser (impl�menter) des combinateurs avec
leurs propri�t�s "alg�briques"

Inspiration : algebra-oriented programming (� chercher sur google)

Les entit�s se combinent entre elles : par exemple, connecter des cuves
avec des "tuyaux" et d'autres entit�s ...

On verra �a aux cours 6, 7, 8 avec les structures alg�briques ...

-}


{-

# Partie 2 : les conteneurs (structures discr�tes)

Fournies par la biblioth�que  containers

-}

{-

Les s�quences : le type Seq a  des listes plus efficaces pour la plupart
des op�rations que le type [a]   (avec des op�rations efficaces en plus)

-}

-- exemple de construction avec Seq.fromList

-- >>> :t Seq.fromList [1, 2, 3, 4, 5 :: Integer]
-- Seq.fromList [1, 2, 3, 4, 5 :: Integer] :: Seq Integer

mySeq :: Seq Text
mySeq = Seq.fromList ["a", "b", "c", "d", "e"]

-- acc�s au premier et au dernier �l�ment

seqFirst :: Seq a -> Maybe a
seqFirst (x :<| _) = Just x
seqFirst _ = Nothing

seqLast :: Seq a -> Maybe a
seqLast (_ :|> x) = Just x
seqLast _ = Nothing

-- >>> seqFirst mySeq
-- Just "a"

-- >>> seqLast mySeq
-- Just "e"

-- Acc�s en O(log(n)) aux indices   avec Seq.index

-- >>> Seq.index mySeq 0
-- "a"
-- >>> Seq.index mySeq 2
-- "c"

-- pour manipuler les s�quences, on peut �crire des fonctions r�cursives
-- mais en "vraie" programmation fonctionnelle on utilise plut�t des combinateurs

-- en particulier : map (plut�t fmap), filter et foldr

carres :: Seq Integer -> Seq Integer
carres xs = fmap (\x -> x * x) xs

-- >>> carres (Seq.fromList [1, 2, 3, 4, 5])
-- fromList [1,4,9,16,25]

-- petit exercice de cours : faire un map sur les s�quences avec une fonction r�cursive

-- foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b 

somme :: Seq Integer -> Integer
somme xs = Seq.foldrWithIndex (\ idx elem res -> res + elem) 0 xs

-- >>> somme (Seq.fromList [1, 2, 3, 4, 5])
-- 15

produit :: Seq Integer -> Integer
produit xs = Seq.foldrWithIndex (\ idx elem res -> res * elem) 1 xs

-- >>> produit (Seq.fromList [1, 2, 3, 4, 5])
-- 120

-- Un bon exo : une fonction qui retourne le maximum dans une s�quence
-- accompagn� de sont indice. ex  maxEtIndice [2, 5, 4, 3, 1]  --> (5, 1)
-- avec forldWithIndex  (et directement, r�cursivement)

{-

## Les tables associatives

Une table d'associations entre des cl�s uniqu�es et une valeur associ�e
Le type des cl� doit �tre ordonn� (typeclass Ord qu'on peut en g�n�ral
d�river ...)

-}

-- exemple de construction : ici � partir du type  [(Text, Integer)]

myMap :: Map Text Integer
myMap = Map.fromList [("a", 1), ("b", 2), ("c", 3), ("d", 4)]

{-

Op�ration de recherche d'une valeur � partir de sa cl� avec Map.lookup

-}

-- >>> Map.lookup (Text.pack "c") myMap
-- Just 3

-- >>> Map.lookup (Text.pack "e") myMap
-- Nothing


{-

Exemple d'op�ration : un fold sur les associations (cl�, valeur)

-}

-- >>> :t Map.foldrWithKey
-- Map.foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b

addLast :: a -> Seq a -> Seq a
addLast x Empty = Empty :|> x    -- petit oublie : Empty est la s�quence vide
addLast x xs = xs :|> x

-- addLast == :|>           <=== donc vous voyez, je dis pas toujours des trucs intelligents

-- >>> addLast (Text.pack "toto") mySeq
-- fromList ["a","b","c","d","e","toto"]

-- Exercice : addFirst


listKeys :: Map k a -> Seq k
listKeys m = Map.foldrWithKey (\cle val res -> res :|> cle) Empty m

-- >>> listKeys myMap
-- fromList ["d","c","b","a"]

-- Exercice : les listVals

{-

## Les ensembles

Structure lin�aire mais sans ordre s�quentiel et sans r�p�tition

-}

-- construction

mySet :: Set Integer
mySet = Set.fromList [1, 2, 1, 3, 3, 5, 4, 5, 1]

-- >>> mySet
-- fromList [1,2,3,4,5]

-- "ajouter" un �l�ment avec insert

-- >>> :t Set.insert
-- Set.insert :: Ord a => a -> Set a -> Set a

-- >>> Set.insert 12 mySet
-- fromList [1,2,3,4,5,12]

-- test d'appartenance

-- >>> Set.member 2 mySet
-- True

-- >>> Set.member 12 mySet
-- False

-- sous-ensemble

-- >>> Set.isSubsetOf mySet (Set.fromList [2, 3, 4])
-- False

-- >>> mySet `Set.union`  (Set.fromList [12, 3, 9])
-- fromList [1,2,3,4,5,9,12]






