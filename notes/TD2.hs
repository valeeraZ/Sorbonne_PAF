module Td2 where

-- Exercice 1

-- >>> head (tail [1])
-- Prelude.head: empty list

-- fonction head est partielle
-- risque de propager l'exception

-- The Maybe datatype provides a way to make a safety wrapper around partial functions,
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe

-- Question 1

safeHead :: [t] -> Maybe t
safeHead [] = Nothing 
safeHead (x:xs) = Just x

-- >>> safeHead [1,2,3]
-- Just 1

-- >>> safeHead (tail [1])
-- Nothing

safeHeadPlus :: [Integer] -> Integer -> Maybe Integer 
safeHeadPlus xs n = case safeHead xs of
    Nothing -> Nothing 
    Just m -> Just (m+n)

-- >>> safeHeadPlus [1, 2, 3] 42
-- Just 43

-- >>> safeHeadPlus [] 42
-- Nothing

-- Question 2

data EmptyList = 
    EmptyList
    deriving Show

eitherHead :: [a] -> Either EmptyList a
eitherHead [] = Left EmptyList
eitherHead (x:xs) = Right x

-- >>> eitherHead [1,2,3]
-- Right 1

-- >>> eitherHead (tail [1])
-- Left EmptyList

eitherHeadPlus :: [Integer] -> Integer  -> Either EmptyList Integer 
eitherHeadPlus xs n = case eitherHead xs of
    Left _ -> Left EmptyList
    Right m -> Right (m+n)

-- >>> eitherHeadPlus [] 3
-- Left EmptyList

-- >>> eitherHeadPlus [1,2,3] 3
-- Right 4

-- Question 3

defaultHead :: [a] -> a -> a
defaultHead [] y = y
defaultHead (x:xs) y = x

defaultHeadPlus :: [Integer] -> Integer -> Integer 
defaultHeadPlus xs n = defaultHead xs 0+n

-- >>> defaultHeadPlus [] 4
-- 4

-- >>> defaultHeadPlus [1,2,3] 4
-- 5

-- Exercice 2

data List a = 
    Nil
    | Cons a (List a) 
    deriving (Show, Eq)

listMap :: (a -> b) -> List a -> List b
listMap _ Nil = Nil
listMap f (Cons e l) = Cons (f e) (listMap f l)

-- fold //reduce: 
-- boucle while en langage impéritif = récursion en langage récursif
-- for boucle = fold

-- fold f a list : appliquer f à chaque élément de list, avec un accumulateur a
-- rendre cet accumulateur

-- >>> :t foldl
-- foldl
--   :: forall (t :: * -> *) b a.
--      Foldable t =>
--      (b -> a -> b) -> b -> t a -> b

-- Pour tout type t
-- si t est foldable
-- (b -> liste -> b) -> b -> liste de type t -> b

-- Question 1

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl f acc Nil = acc
listFoldl f acc (Cons x xs) = listFoldl f (f acc x) xs

-- >>> listFoldl (\x y -> x + y) 0 $ Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil
-- 10

-- Question 2

listMap2 :: (a -> b) -> List a -> List b
listMap2 f = listFoldl aux Nil
    where aux ys z = Cons (f z) ys

-- >>> listMap2 (\x -> x + 3) $ Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil
-- Cons 7 (Cons 6 (Cons 5 (Cons 4 Nil)))

-- mais c'est l'inverse! il faut un listFoldr pour une autre direction

listFoldr :: (b -> a -> b) -> b -> List a -> b
listFoldr f acc Nil = acc
listFoldr f acc (Cons x xs) = f (listFoldr f acc xs) x

listMap3 :: (a -> b) -> List a -> List b
listMap3 f = listFoldr aux Nil 
    where aux ys z = Cons (f z) ys

-- >>> listMap3 (\x -> x + 3) $ Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil
-- Cons 4 (Cons 5 (Cons 6 (Cons 7 Nil)))

prop_lmap :: Eq b => (a->b) -> List a -> Bool 
prop_lmap f xs = listMap f xs == listMap2 f xs

-- pour comparer des data il faut dériver Eq

-- Exercice 3

data BTTree a = 
    Leaf
    | Two a (BTTree a) (BTTree a)
    | Three a (BTTree a) (BTTree a) (BTTree a)
    deriving Show

nbTwo :: BTTree a -> Int
nbTwo Leaf = 0
nbTwo (Two v fg fd) = nbTwo fg + nbTwo fd + 1
nbTwo (Three v fg fm fd) = nbTwo fm + nbTwo fg + nbTwo fd

exemple :: BTTree Int
exemple = Three 12 
                (Two 11 Leaf (Two 10 Leaf Leaf))
                (Three 8 Leaf (Two 9 Leaf Leaf) Leaf)
                Leaf

-- >>> nbTwo exemple
-- 3

depth :: BTTree a -> Int 
depth Leaf = 0
depth (Two _ fg fd) = max (depth fg) (depth fd) + 1
depth (Three _ fg fm fd) = max (depth fm) (max  (depth fd) (depth fg)) + 1

-- >>> depth exemple
-- 3

prefixe :: BTTree a -> [a]
prefixe Leaf = []
prefixe (Two v fg fd) = v : prefixe fg ++ prefixe fd
prefixe (Three v fg fm fd) = v : prefixe fg ++ prefixe fm ++ prefixe fd

-- >>> prefixe exemple
-- [12,11,10,8,9]

treeMap :: (a -> b) -> BTTree a -> BTTree b
treeMap _ Leaf = Leaf
treeMap f (Two v fg fd) = Two (f v) (treeMap f fg) (treeMap f fd)
treeMap f (Three v fg fm fd) = Three (f v) (treeMap f fg) (treeMap f fm) (treeMap f fd)

-- >>> prefixe $ treeMap (\x -> x+ 10) exemple
-- [22,21,20,18,19]

-- une fonction pour arbre binaire
-- une fonction pour arbre ternaire
-- un accumulateur
-- l'arbre soi même
-- résultat en meme type de l'accumulateur (type b)
treeFold :: (b -> b -> a -> b) -> (b -> b -> b -> a -> b)-> b -> BTTree a -> b
treeFold _ _ acc Leaf = acc
treeFold f2 f3 acc (Two v fg fd) = f2 (treeFold f2 f3 acc fg) (treeFold f2 f3 acc fd) v
treeFold f2 f3 acc (Three v fg fm fd) = f3 (treeFold f2 f3 acc fg) (treeFold f2 f3 acc fm) (treeFold f2 f3 acc fd) v

-- récrire, avec treeFold
nbTwo2 :: BTTree a -> Int
nbTwo2 = treeFold (\ag ad _ -> 1 + ag + ad) (\ag am ad _ -> 1 + ag + am + ad) 0

depth2 :: BTTree a -> Int
depth2 = treeFold (\ag ad _ -> 1 + max ag ad) (\ag am ad _ -> 1 + max ag (max am  ad)) 0

prefixe2 :: BTTree a -> [a]
prefixe2 = treeFold (\ag ad v -> v : ag ++ ad) (\ag am ad v -> v : ag ++ am ++ ad) []

-- >>> prefixe2 exemple
-- [12,11,10,8,9]
