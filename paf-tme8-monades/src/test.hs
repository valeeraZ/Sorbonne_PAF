module Test where

import PAF_TME8_GenMonad
import Test.QuickCheck
import System.Random (Random)

-- >>> samples 4 (arbitrary :: Gen [Integer])
-- [[],[0,-2],[3,-1,1,-4],[-4,-2]]

-- >>> samples 10 (arbitrary :: Gen (Maybe Int))
-- [Just 0,Just (-1),Nothing,Nothing,Just (-3),Just (-6),Just 2,Just (-5),Just (-9),Nothing]

-- >>> samples 3 (arbitrary :: Gen (Double, Bool))
-- [(0.0,True),(-0.4107604927225674,False),(-1.5497909914430976,True)]

-- >>> samples 4 (arbitrary :: Gen [Either Int (Maybe Bool)])
-- [[],[],[Left 2,Right (Just True),Right (Just True)],[Left (-1),Right (Just True),Right (Just False),Left (-4),Left 1]]

-- >>> sample' $ choose ((4,10)::(Integer, Integer))
-- [5,6,9,9,6,7,4,4,8,4,10]

-- >>> sample' $ choose ('a', 'z')
-- "wbwkaqvrmgx"

-- >>> sample' $ chooseNat (10::Integer)
-- [5,6,10,5,7,5,10,2,6,5,2]

-- >>> sample' $ elements [42, 2, 17]
-- [17,42,42,17,17,42,2,2,17,2,2]

-- >>> samples 5 $ listOf (chooseNat (10::Integer))
-- [[],[1,5],[10,8,7,6],[2,7],[5,10,7,5,4,1]]

-- >>> sample' $ elements ['a' .. 'e']
-- "aadaaceacee"

-- >>> samples 3 $ resize 10 $ listOf (chooseNat (10::Integer))
-- [[4,5,1,4,9],[4,1,3,4,10,2,8,4],[10,3,1]]

-- >>> sample' $ resize 10 $ choose ('a', 'z')
-- "tqgkkxtqzwf"

-- >>> sample' $ oneof [choose (1::Integer, 10::Integer), choose (100::Integer, 110::Integer), choose (1000::Integer, 1010::Integer)]
-- [1010,1005,4,105,1004,7,109,1004,1004,1001,100]

-- >>> sample' $ frequency [(60, choose (1::Integer, 10::Integer)), (30, choose (100::Integer, 110::Integer)), (10, choose (1000::Integer, 1010::Integer))]
-- [108,5,6,108,4,3,103,8,4,10,106]

-- >>> checkFrequency $ resize 100000 $ listOf genFreq
-- (60.26546982429335,29.679144385026735,10.055385790679908)

-- >>> sample' $ chooseNat (100::Integer) `suchThat` even
-- [68,42,34,2,36,4,74,64,60,80,76]

-- >>> sample' $ (pure 42 :: Gen Int)
-- [42,42,42,42,42,42,42,42,42,42,42]

-- >>> sample' $ chooseInv $ chooseNat (10::Integer)
-- [-3,-1,-2,-5,-3,-8,-5,-10,-6,-5,-5]

-- >>> sample' $ chooseInv2 $ chooseNat (10::Integer)
-- [-7,-10,-3,-2,-1,-9,-8,-7,-1,-10,-2]

-- >>> samples 5 $ genPair (chooseNat (10::Integer)) (arbitrary :: Gen Bool)
-- [(7,True),(5,True),(1,True),(4,True),(6,True)]

-- >>> samples 5 $ genPair2 (chooseNat (10::Integer)) (arbitrary :: Gen Bool)
-- [(1,True),(9,False),(5,False),(8,True),(7,True)]

-- >>> samples 2 $ genPersonne
-- [Personne {nom = "bxbuzxvhxy", prenom = "fmpadaipyi", age = 81},Personne {nom = "hrmgjuduce", prenom = "qhpajwmjqp", age = 12}]

-- >>> samples 6 $ genMaybe $ choose (1::Integer, 10::Integer)
-- [Just 9,Nothing,Just 5,Nothing,Nothing,Nothing]

-- >>> samples 3 genGeom
-- [Rect {longueur = 60, largeur = 18},Rect {longueur = 30, largeur = 13},Circle {rayon = 8}]

-- >>> samples 1 $ resize 10 $ genNat
-- [S (S (S (S Z)))]

-- >>> samples 4 $ listOfSize (chooseNat (10::Integer)) 5
-- [[2,7,6,10,8],[2,10,4,3,8],[2,7,1,1,5],[5,6,5,1,9]]

-- >>> samples 4 $ resize 5 $ sizedList $ chooseNat (10::Integer)
-- [[10,7,6,7,8],[9,9,4,4,3],[8,1,2,6,5],[5,3,5,4,6]]

-- >>> samples 4 $ resize 5 $ sizedList' $ chooseNat (10::Integer)
-- [[7,1,2,6,5],[1,8,6,4,6],[10,5,5,2,8],[8,10,10,6,10]]

-- >>> samples 1 $ genBinTreeNaive (choose (1::Integer, 5::Integer)) 5
-- [Node 1 (Node 5 (Node 5 Tip Tip) (Node 1 Tip (Node 2 (Node 2 Tip Tip) (Node 1 Tip Tip)))) (Node 1 Tip (Node 2 (Node 1 Tip Tip) (Node 5 Tip Tip)))]

-- >>> samples 1 $ resize 5 $ genBinTree (choose (1::Integer, 5::Integer))
-- [Node 3 Tip (Node 5 (Node 4 (Node 3 Tip Tip) (Node 3 Tip Tip)) (Node 4 (Node 4 Tip Tip) (Node 3 (Node 3 Tip Tip) (Node 5 Tip Tip))))]
