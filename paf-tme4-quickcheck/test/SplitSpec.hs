module SplitSpec where

import Test.Hspec
import Test.QuickCheck

import Split

splitSpec0 = do
  describe "split" $ do
    it "splits a string wrt. a given character into a list of words" $
      (split '/' "aa/bb/ccc/dd d") `shouldBe` ["aa", "bb", "ccc", "dd d"] -- tester la fonctionnalité principale

splitSpec1 = do
  describe "split" $ do
    it "can be undone with unsplit (v1)" $ property $
      \c xs -> collect (length xs) $ prop_split_unsplit c xs 
      -- affiche la taille de chaîne
      -- tester la propriété avec c et xs


splitSpec2 = do
  describe "split" $ do
    it "can be undone with unsplit (v2)" $ property $
      \xs -> forAll (elements xs) $ \c -> collect (length xs) $ prop_split_unsplit c xs

-- Remarque : on utilise comme caractère de split les éléments des listes `xs` en entrée,
--            cf. la doc QuickCheck sur `forAll`, `elements`, etc.
-- elements: générer une valeur dans la liste donnée comme argument
-- forAll: prendre le générateur (elements) et une fonction (a -> prop) (ici: \c -> collect (length xs))
-- pour faire des tests

splitSpec3 = do
  describe "split" $ do
    it "can be undone with unsplit (v3)" $ property $
      forAll (oneof [return "bla bla bli"
                     , return "toto"
                     , return ""
                     , return "un    deux trois   quatre"]) $
      \xs -> prop_split_unsplit ' ' xs

