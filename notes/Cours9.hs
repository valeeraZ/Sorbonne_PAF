
{-# LANGUAGE ScopedTypeVariables, InstanceSigs
             , TypeSynonymInstances, FlexibleInstances
             , BangPatterns, MultiParamTypeClasses #-}

module Cours9 where

import Control.Applicative
import Control.Monad

{-{

# PAF 9 : Composer des monades avec les transformers

La question qu'on se pose maintenant est la suivante :

> Comment composer des contextes (fonctoriels, applicatifs
  et surtout monadiques) ?


}-}

{-{

## Première approche : la composition ad-hoc

Pour notre exemple, on considère trois contextes :

 - les calculs optionnels avec `Maybe`
 - le contexte `Writer` pour (entre autre) faire du logging (traces d'exécutions)
 - le contexte "magique" de `IO` pour les entrées/sorties

Pour commencer, je donne en bloc une implémentation de `Writer`.

}-}

newtype Writer e a = Writer { runWriter :: (a, e) }
  deriving Show

instance Functor (Writer e) where
  fmap g (Writer (x, !y)) = Writer (g x, y)

instance Monoid e => Applicative (Writer e) where
  pure x = Writer (x, mempty)
  (Writer (g, !y1)) <*> (Writer (x, !y2)) = Writer (g x, y1 <> y2)

instance Monoid e => Monad (Writer e) where
  (>>=) :: Writer e a -> (a -> Writer e b) -> Writer e b
  (Writer (x, !y1)) >>= f = case f x of
                              Writer (x', !y2) -> Writer (x', y1 <> y2)

-- Exemples :

tell :: [a] -> Writer [a] ()
tell xs = Writer ((), xs)

logg :: a -> Writer [a] ()
logg x = tell [x]

type Log = [String]

incrWithLog :: Integer -> Writer Log Integer
incrWithLog x = do
  logg $ "Increment de " <> (show x)
  return $ x + 1

addWithLog :: Integer -> Integer -> Writer Log Integer
addWithLog x y = do
  logg $ "Addition de " <> (show x) <> " et " <> (show y)
  return $ x + y

divWithLog :: Integer -> Integer -> Writer Log Integer
divWithLog x y = do
  logg $ "Division de " <> (show x) <> " par " <> (show y)
  return $ x `div` y

-- >>> incrWithLog 10
-- Writer {runWriter = (11,["Increment de 10"])}

-- >>> addWithLog 39 3
-- Writer {runWriter = (42,["Addition de 39 et 3"])}

-- >>> fmap even (addWithLog 39 3)
-- Writer {runWriter = (True,["Addition de 39 et 3"])}

-- >>> (+) <$> (addWithLog 39 3) <*> (incrWithLog 10)
-- Writer {runWriter = (53,["Addition de 39 et 3","Increment de 10"])}


-- Considérons maintenant une opération composée

complexOp :: Integer -> Writer Log Integer
complexOp n = do
  x <- incrWithLog n
  y <- addWithLog n x
  divWithLog y (x `div` 20)

-- >>> complexOp 42
-- Writer {runWriter = (42,["Increment de 42","Addition de 42 et 43","Division de 85 par 2"])}


-- Remarque : pour faire des logs "en production",
-- cf par ex.: https://hackage.haskell.org/package/fast-logger

{-

On remarque que l'on utilise une fonction partielle ...

-}

-- >>> divWithLog 4 0
-- Writer {runWriter = (*** Exception: divide by zero

-- >>> complexOp 10
-- Writer {runWriter = (*** Exception: divide by zero

{-

On aimerait donc rendre la division plus sûre ...

-}

divSafe :: Integer -> Integer -> Maybe Integer
divSafe x y = do
  guard (y /= 0)
  return $ x `div` y

-- ou, si vous préférez ...
-- divSafe x y
--    | y == 0 = Nothing
--    | otherwise = Just $ x `div` y

-- >>> 8 `divSafe` 2
-- Just 4


-- >>> 8 `divSafe` 0
-- Nothing

{-

Voici donc une nouvelle version "safe" des opérations avec log :

-}

divWithLogSafe :: Integer -> Integer -> Writer Log (Maybe Integer)
divWithLogSafe x y = do
  logg $ "Division de " <> (show x) <> " par " <> (show y)
  return $ x `divSafe` y

complexOpSafe :: Integer -> Writer Log (Maybe Integer)
complexOpSafe n = do
  x <- incrWithLog n
  y <- addWithLog n x
  divWithLogSafe y (x `div` 20)

-- >>> complexOpSafe 42
-- Writer {runWriter = (Just 42,["Increment de 42","Addition de 42 et 43","Division de 85 par 2"])}


-- >>> complexOpSafe 10
-- Writer {runWriter = (Nothing,["Increment de 10","Addition de 10 et 11","Division de 21 par 0"])}


{-

**Remarque importante** :  ici, le contexte est `Writer Log Maybe`
on a emboîté la monade `Maybe` à l'intérieur de `Writer`.
l'interprétation est : le traçage de calculs optionnels

Si on utilise plutôt le contexte `Maybe (Writer Log)`
l'interprétation est : des calculs tracés optionnels, ce qui n'a
pas grand chose à voir (et est moins intéressant en pratique...).

-}


{-{

## Plongement dans IO

Rappel : `IO` est une monade (magique)

Une petite fonction utilitaire :

}-}

readInt :: String -> Maybe Integer
readInt str = case (reads str) :: [(Integer,String)] of
                [(k, "")] -> Just k
                _ -> Nothing

-- >>> readInt "42"
-- Just 42

-- >>> readInt "abc"
-- Nothing

complexOpSafeIO :: IO (Writer Log (Maybe Integer))
complexOpSafeIO = do  -- ici ce `do` est dans IO
  putStr "n = "
  str <- getLine
  return $
    case readInt str of
      Nothing -> return Nothing
      Just n -> do -- ici ce `do` est dans Writer
        logg $ "Lecture de n=" <> (show n)
        complexOpSafe n

-- A tester dans GHCI

{-

Petit bilan :

1) Composer des monades est nécessaires en pratique, ne serait-ce que IO
 et une autre monade.

2) En terme de typage :   composer ~ emboîter  (les contextes)
   ... et l'ordre est donc important (en général)

3) A partir de 3 monades emboîtés (dont IO) les différents niveaux de `do`
commencent à être un peu fastidieux à gérer...

-}


{-

Plusieurs solutions :  bibliothèques "d'effets"  (fused-effects, polysemy, freer-simple, ...)
 et sinon  ==> les monad transformers 

Il y a deux bibliothèques "officielles" :

   - transformers  qui une version "light"  des monad transformers

   - mtl  (monad template library) qui est une verson "heavy" avec beaucoup
     d'automatisation

Pour le cours : je veux rester au niveau des concepts et donc on va juste
comprendre l'architecture de base de "transformers".

-}

{-{

## Vers une composition systématique ?

La question est :

> Les contextes (fonctoriels, applicatifs, monadiques) sont-ils composables ?

On peut introduire un mécanisme de composition de contexte :

}-}

newtype Compose t m a = Compose { getCompose :: t (m a) }
  deriving Show

{-{

-- >>> :kind Compose
-- Compose :: (* -> *) -> (* -> *) -> * -> *


à comparer à :

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c


Exemple de valeur dans ce contexte :

-- >>> Compose $ Writer (Just (42 :: Integer), ["Au debut c'est 42"])
-- Compose {getCompose = Writer {runWriter = (Just 42,["Au debut c'est 42"])}}


-- >>> :t Compose $ Writer (Just (42 :: Integer), ["Au debut c'est 42"])
-- Compose $ Writer (Just (42 :: Integer), ["Au debut c'est 42"])
--   :: Compose (Writer [[Char]]) Maybe Integer


-- ...
...  :: Compose (Writer Log)  Maybe   Integer
                \__________| |_____| |______|
--                   t          m       a


Autres exemples :

>>> Compose (Just [1..4::Integer])
Compose {getCompose = Just [1,2,3,4]}

>>> :t Compose (Just [1..4::Integer])
... :: Compose Maybe  []  Integer
--             \___| |_| |______|
--               t    m     a


>>> Compose (fmap Just [1..4::Integer])
Compose {getCompose = [Just 1,Just 2,Just 3,Just 4]}

>>> :t Compose (fmap Just [1..4::Integer])
... :: Compose []   Maybe  Integer
--            \_|  |____| |______|
--             t     m       a


Remarque : on voit encore une fois que les contextes ne commutent
pas (en général)

}-}


{-{

Question : est-ce Compose "passe" au contexte ?

 1. est que le Compose de deux foncteurs est un foncteur ?
 2. est que le Compose de deux applicatifs est un applicatif ?
 3. est que le Compose de deux monades est une monade ?

==> Oui pour 1. et 2.    (cf. TD)
==> Non pour 3.

}-}

{-

On ne peut pas implémenter, dans le cas général, l'instance suivante :

instance (Monad t, Monad m) => Monad (Compose t m) where
  -- (>>=) :: Compose t m a -> (a -> (Compose t m b)) -> (Compose t m b)
  x >>= f = undefined

Par contre on peut instancier `Functor` et `Applicative` pour `Compose t m`
(cf. TD)

-}


{-{

## Une solution un peu moins générique : les transformers

L'idée de base des transformers est assez triviale.

Par exemple on veut emboîter Maybe dans une autre monade :

 - avec `Writer` on utiliserait le contexte  `Writer Log (Maybe a)`
 - avec `State`  ce serait `State St (Maybe a)`
 - avec `IO` ce serait `IO (Maybe a)`

Donc pour une monade quelconque `m`,  on passe de `(Maybe a)` à `m (Maybe a)` 

Le transformer pour `MaybeT` effectue cette généralisation sous le forme d'un type
paramétré.

}-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- Remarque : on ne peut pas dériver `Show` car on ne connaît
-- pas `m`

-- Notre objectif : rendre cette construction monadique

-- 1) pour foncteur

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap g (MaybeT x) = MaybeT $ fmap (fmap g) x

{-
On a :
g :: a -> b
x :: m (Maybe a)
et : _goal :: MaybeT m b

remarque : Maybe est un Functor, donc :

fmap/Maybe :: (a -> b) -> Maybe a -> Maybe b

(fmap/Maybe g) :: Maybe a -> Maybe b

On a un seul moyen de "franchir" le contexte m :

fmap/m :: (u -> v) -> m u -> m v
Si on pose : u=Maybe a et v=Maybe b

donc :

fmap/m (fmap/Maybe g) :: m Maybe a -> m Maybe b

on a gagné !

-}


-- Exemples d'applications :

-- >>> runMaybeT $ fmap (*2) (MaybeT (Writer (Just 42, ["Valeur 42"])))
-- Writer {runWriter = (Just 84,["Valeur 42"])}

-- >>> :t runMaybeT $ fmap (*2) (MaybeT (Writer (Just (42 :: Integer), ["Valeur 42"])))
-- runMaybeT $ fmap (*2) (MaybeT (Writer (Just (42 :: Integer), ["Valeur 42"])))
--   :: Writer [[Char]] (Maybe Integer)

-- >>> runMaybeT $ fmap (*2) (MaybeT (fmap Just [1..4 :: Integer]))
-- [Just 2,Just 4,Just 6,Just 8]

-- >>> :t runMaybeT $ fmap (*2) (MaybeT (fmap Just [1..4 :: Integer]))
-- runMaybeT $ fmap (*2) (MaybeT (fmap Just [1..4 :: Integer]))
--   :: [Maybe Integer]

-- Pour applicative :

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

{-

x :: a
_goal :: m Maybe a

pure/Maybe :: a -> Maybe a

donc pure/Maybe x :: Maybe a   ... ou   pure x :: Maybe a

et pure/m :: u -> m u
on pose : u=Maybe a

-- on a gagné !

-}
  
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT g) <*> (MaybeT x) = MaybeT $ (fmap (<*>) g) <*> x

{-
g :: m Maybe (a -> b)
x :: m Maybe a

_goal :: m Maybe b

<*>/Maybe :: Maybe (a -> b) -> Maybe a -> Maybe b

<*>/m :: m (u -> v) -> m u -> m v
   avec  u=Maybe a  et v=Maybe b

donc on cherche une fonction du type:   m (Maybe a -> Maybe b)

_goal2 :: m (Maybe a -> Maybe b)

Rappel :
<*>/Maybe :: Maybe (a -> b) -> Maybe a -> Maybe b

g :: m Maybe (a -> b)

fmap/m :: (x -> y) -> m x -> m y
avec x = Maybe (a -> b)
et y = Maybe a -> Maybe b

fmap/m (<*>/Maybe)  :: m  Maybe (a -> b) -> m (Maybe a -> Maybe b)


-}


-- Exemples :

safeVal :: (Num a, Show a) => a -> Writer Log (Maybe a)
safeVal x = Writer (Just x, ["Valeur " <> (show x)])

-- >>> runMaybeT $ (*) <$> (MaybeT (safeVal 13)) <*> (MaybeT (safeVal 3))
-- Writer {runWriter = (Just 39,["Valeur 13","Valeur 3"])}

-- Finalement, le contexte monadique :

-- >>> runMaybeT $ (*) <$> (MaybeT (fmap Just [1,2,3,4])) <*> (MaybeT ([Nothing] <> (fmap Just [2, 10])))
-- [Nothing,Just 2,Just 10,Nothing,Just 4,Just 20,Nothing,Just 6,Just 30,Nothing,Just 8,Just 40]

-- il nous reste le contexte monadique

instance Monad m => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  v >>= f =  MaybeT $ do
    x <- runMaybeT v
    case x of
      Nothing -> pure Nothing
      Just y -> runMaybeT (f y)

-- v :: MaybeT m a
-- f :: (a -> MaybeT m b)
-- x :: Maybe a

-- Maintenant, on va utiliser cette construction pour recoder notre
-- exemple (ou le rendre plus élégant)

divWithLogSafeM :: Integer -> Integer -> MaybeT (Writer Log) Integer
divWithLogSafeM x y = MaybeT $ do
  logg $ "Division de " <> (show x) <> " par " <> (show y)
  return $ x `divSafe` y

complexOpSafeM :: Integer -> MaybeT (Writer Log) Integer
complexOpSafeM n = MaybeT $ do
  x <- incrWithLog n
  y <- addWithLog n x
  runMaybeT $ divWithLogSafeM y $ x `div` 20

{-

Un petit exemple :

-}

ioDivSafe :: MaybeT IO Integer   -- en interne : IO (Maybe Integer)
ioDivSafe = MaybeT $ do -- contexte de IO
  putStr "Numérateur = "
  strx <- getLine
  let x = readInt strx
  putStr "Dénominateur = "
  stry <- getLine
  let y = readInt stry
  if y == Just 0
  then return Nothing
  else return $ div <$> x <*> y

{-

On aimerait rester dans un seul niveau de `do`  (en fait celui du transformer
`MaybeT` et faire "remonter" (lifter) les autres contextes (ici IO)

Pour cela, en Haskell, on utilise la typeclasse suivante :

-}

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  --lift x = MaybeT (fmap Just x)
  lift = MaybeT . (fmap Just)

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

{-
ioDivSafe2 :: MaybeT IO Integer
ioDivSafe2 = do -- contexte de MaybeT
  lift $ putStr "Numérateur = "
  strx <- lift $ getLine
  x <- hoistMaybe $ readInt strx
  lift $ putStr "Dénominateur = "
  stry <- lift $ getLine
  y <- hoistMaybe $ readInt stry
  if y == 0
  then hoistMaybe Nothing
  else return $ x `div` y
-}

{-

par exemple :

>>> runMaybeT ioDivSafe2
Numérateur = 39
Dénominateur = 3
Just 13

>>> runMaybeT ioDivSafe2
Numérateur = 4
Dénominateur = 0
Nothing
  
-}



