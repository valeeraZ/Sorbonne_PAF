{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless, foldM)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map
import qualified Data.Map as Map

import Data.Sequence
import qualified Data.Sequence as Seq

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import qualified SDL.Font as F
import qualified SDL.Video.Renderer as R
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Data.Text as T

import GHC.Base (when)

import Coord
import Environnement
--import qualified Environnement as Env
import Niveau
import qualified Niveau as N
import Etat
import Moteur
import Lemming
import Control.Arrow ((***))

import Debug.Trace as Tr
import Mouse


tailleBloc :: CInt
tailleBloc = 50

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1280 720)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadElement :: Renderer -> FilePath -> String -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadElement rdr path id tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId id) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id) (S.mkArea 0 0 tailleBloc tailleBloc)
  let smap' = SM.addSprite (SpriteId id) sprite smap
  return (tmap', smap')

affiche :: Renderer -> TextureMap -> SpriteMap -> String -> [Coord]  -> IO ()
affiche _ _ _ _ [] = return ()
affiche renderer tmap smap id ((C x y):as) = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId id) smap) (fromIntegral x * tailleBloc) (fromIntegral y *tailleBloc) )
  affiche renderer tmap smap id as

displayMap :: Renderer -> TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO ()
displayMap renderer tmap smap ht lg transx transy map = do
  displayTerre renderer tmap smap map transx transy
  displayMetal renderer tmap smap map transx transy
  displayEntree renderer tmap smap map transx transy
  displaySortie renderer tmap smap map transx transy
  return ()

displayTerre :: Renderer -> TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayTerre renderer tmap smap map transx transy = do
  let terreList = Map.keys $ filterWithKey (\k v -> Just v == Just Terre) map
  affiche renderer tmap smap "terre" terreList

displayMetal :: Renderer -> TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayMetal renderer tmap smap map transx transy = do
  let metalList = Map.keys $ filterWithKey (\k v -> Just v == Just Metal) map
  affiche renderer tmap smap "metal" metalList

displayEntree :: Renderer -> TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayEntree renderer tmap smap map transx transy = do
  let entreeList = Map.keys $ filterWithKey (\k v -> Just v == Just Entree) map
  affiche renderer tmap smap "entree" entreeList

displaySortie :: Renderer -> TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displaySortie renderer tmap smap map transx transy = do
  let sortieList = Map.keys $ filterWithKey (\k v -> Just v == Just Sortie) map
  affiche renderer tmap smap "sortie" sortieList

displayLemmings :: Renderer -> TextureMap -> SpriteMap -> Map Coord (Seq Entite) -> CInt -> CInt -> IO ()
displayLemmings renderer tmap smap lemmings transx transy = do
  let marcheursDroite = Map.keys $ filterWithKey (\k v -> not $ Seq.null $ Seq.filter isMarcheurDroite v) lemmings
  let marcheursGauche = Map.keys $ filterWithKey (\k v -> not $ Seq.null $ Seq.filter isMarcheurGauche v) lemmings
  let creuseurs = Map.keys $ filterWithKey (\k v -> not $ Seq.null $ Seq.filter isCreuseur v) lemmings
  let poseursDroite = Map.keys $ filterWithKey (\k v -> not $ Seq.null $ Seq.filter isPoseurDroite v) lemmings
  let poseursGauche = Map.keys $ filterWithKey (\k v -> not $ Seq.null $ Seq.filter isPoseurGauche v) lemmings
  let stoppeurs = Map.keys $ filterWithKey (\k v -> not $ Seq.null $ Seq.filter isStoppeur v) lemmings
  affiche renderer tmap smap "lemming-d" marcheursDroite
  affiche renderer tmap smap "lemming-g" marcheursGauche
  affiche renderer tmap smap "creuseur" creuseurs
  affiche renderer tmap smap "poseur-d" poseursDroite
  affiche renderer tmap smap "poseur-g" poseursGauche
  affiche renderer tmap smap "stoppeur" stoppeurs

displayMonstres :: Renderer -> TextureMap -> SpriteMap -> Map Coord (Seq Entite) -> CInt -> CInt -> IO ()
displayMonstres renderer tmap smap entites transx transy = undefined

-- |afficher entites (Lemmings & Monstres)
displayEntites :: Renderer -> TextureMap -> SpriteMap -> Map Coord (Seq Entite) -> CInt -> CInt -> IO ()
displayEntites renderer tmap smap entites transx transy = do
  let lemmingsMap = filterWithKey (\k v -> not $ Seq.null $ Seq.filter isLemming v) entites
  displayLemmings renderer tmap smap lemmingsMap transx transy
  -- displayMonstres si possible à faire après
  return ()

displaySelected :: Renderer -> TextureMap -> SpriteMap -> Envi -> Int -> CInt -> CInt -> IO ()
displaySelected renderer tmap smap envi id transx transy
  | id > 0 = do
        case trouveIdSeq id (entitesEnvi envi) of
          Just entite -> do
              putStrLn $ show entite
              let coord@(C x y) = coordP entite in
                S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "selected") smap) (fromIntegral x * tailleBloc) (fromIntegral y *tailleBloc) )
          Nothing -> return ()   
  |  otherwise = do
      --Tr.traceIO "nothing"
      return ()

displayText :: Renderer -> String -> CInt -> IO (CInt)
displayText renderer str height = do
  F.initialize
  let color = V4 0 0 0 0
  font <- F.load "assets/OpenSans-Regular.ttf" 20
  let text = T.pack str
  fontSize <- F.size font text
  let (w, h) = (fromIntegral *** fromIntegral) fontSize
  surface <- F.blended font color text
  --putStrLn ("w:" ++ show w ++ ", h:" ++ show h)
  F.free font
  F.quit
  texture <- createTextureFromSurface renderer surface
  R.freeSurface surface
  copy renderer texture Nothing (Just (Rectangle (P $ V2 (1280-w) height) (V2 w h)))
  return (height + h)

displayTitle :: Renderer -> String -> String -> IO ()
displayTitle renderer title subtitle = do
  F.initialize
  let color = V4 255 255 255 0
  -- big title: 80 size
  font <- F.load "assets/OpenSans-Regular.ttf" 80
  let textTitle = T.pack title
  fontSize <- F.size font textTitle
  let (w1, h1) = (fromIntegral *** fromIntegral) fontSize
  surfaceTitle <- F.blended font color textTitle
  F.free font
  F.quit

  -- small sub title: 40 size
  F.initialize
  font <- F.load "assets/OpenSans-Regular.ttf" 40
  let textSubtitle = T.pack subtitle
  fontSize <- F.size font textSubtitle
  let (w2, h2) = (fromIntegral *** fromIntegral) fontSize
  surfaceSubtitle <- F.blended font color textSubtitle
  F.free font
  F.quit
  textureTitle <- createTextureFromSurface renderer surfaceTitle
  textureSubtitle <- createTextureFromSurface renderer surfaceSubtitle
  R.freeSurface surfaceTitle
  R.freeSurface surfaceSubtitle
  copy renderer textureTitle Nothing (Just (Rectangle (P $ V2 ((1280 - w1) `div` 2) ((720 `div` 2) - h1) ) (V2 w1 h1)))
  copy renderer textureSubtitle Nothing (Just (Rectangle (P $ V2 ((1280 - w2) `div` 2) (720 `div` 2) ) (V2 w2 h2)))

-- |afficher les données du jeu (nombre de lemmings restants/sauvés/vivants ...), un par un en haut à droite de fenêtre
displayData :: Renderer -> Int -> Int -> Int -> IO ()
displayData renderer lrestants lviviants lsauves = do
  h <- displayText renderer ("Nombre de lemmings restants:" ++ show lrestants) 0
  h <- displayText renderer ("Nombre de lemmings vivants:" ++ show lviviants) h
  h <- displayText renderer ("Nombre de lemmings sauvés:" ++ show lsauves) h
  h <- displayText renderer "Cliquer un lemming pour l'attrbuer une classe:'" h
  h <- displayText renderer "Appuyer sur M: Marcheur" h
  h <- displayText renderer "Appuyer sur C: Creuseur" h
  h <- displayText renderer "Appuyer sur P: Poseur" h
  h <- displayText renderer "Appuyer sur S: Stoppeur" h
  displayText renderer "Appuyer sur Échapper: quitter le jeu" h
  return ()

displayEtat :: Renderer -> TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> Etat -> IO ()
displayEtat renderer tmap smap ht lg transx transy etat@(Etat envi niveau lrestants lviviants lsauves lemmingSelected) = do
  let (Niveau h l map) = niveau
  let (Envi h l entites cases) = envi
  --- display map
  displayMap renderer tmap smap ht lg transx transy map
  --- display Entites(Lemmings/Monstres)
  displayEntites renderer tmap smap cases transx transy
  --- display donneés du jeu
  displayData renderer lrestants lviviants lsauves
  --- display le lemming selectionné
  displaySelected renderer tmap smap envi lemmingSelected transx transy

  return ()


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 1280 720 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- liste d'éléments (Texture ID, FilePath) à charger
  let elementsList = [("lemming-d", "assets/lemming-droite.bmp"), ("lemming-g", "assets/lemming-gauche.bmp"), 
                                       ("poseur-d", "assets/poseur-droite.bmp"), ("poseur-g", "assets/poseur-gauche.bmp"),
                                       ("creuseur", "assets/creuseur.bmp"), ("stoppeur", "assets/stoppeur.bmp"),
                                       ("selected", "assets/lemming-selected.bmp"), ("terre", "assets/terre.bmp"),
                                       ("metal", "assets/metal.bmp"), ("entree", "assets/entree.bmp"),
                                       ("sortie", "assets/sortie.bmp")]
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement des images
  (tmap, smap) <- foldM (\(t,s) (id, path) -> loadElement renderer path id t s ) (tmap, smap) elementsList
  -- chargement de la carte
  niveau <- N.niveauDeFichier "assets/map.txt"
  let (Niveau h l map) = niveau
  -- initialisation de l'état du jeu
  --let gameState = M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  let etat = Etat (envide h l) niveau 10 0 0 0
  startLoop 1 renderer tmap smap kbd 0 etat
      
startLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard  -> Int -> Etat -> IO ()
startLoop frameRate renderer tmap smap kbd tour etat = do
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  displayTitle renderer "Bienvenue!" "Appuyer sur S pour commencer"
  present renderer
  if K.keypressed KeycodeS kbd'
    then gameLoop frameRate renderer tmap smap kbd tour etat Nothing
    else startLoop frameRate renderer tmap smap kbd tour etat
  

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard  -> Int -> Etat -> Maybe Fin -> IO ()
gameLoop frameRate renderer tmap smap kbd tour etat@(Etat envi niveau lrestants lviviants lsauves lemmingSelected) fini = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let m' = Mouse.handleEvents events Nothing
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display etat
  displayEtat renderer tmap smap 0 0 0 0 etat
  --- display fin
  case fini of
        Just (Victoire sauve) -> do
          putStrLn "VICTOIRE"
          displayTitle renderer "Victoire!" ("Vous avez sauvé " ++ show sauve ++ " lemmings")
        Just Defaite -> do 
          putStrLn "DÉFAITE"
          displayTitle renderer "Défaite!" "Une moitié de lemmings sont morts"
        Nothing -> return ()
  
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  putStrLn ("mouse: " ++ show m')
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  -- when (M.insideGameState mouse gameState)
  --  (putStrLn "Touched !")
  --- update du game state
  -- let gameState' = M.gameStep gameState kbd' deltaTime
  ---

  unless (K.keypressed KeycodeEscape kbd')
    (do
        putStrLn ("tour " ++ show tour)
        e1 <- tourMoteur tour etat m' kbd'
        case e1 of
          Left fin -> gameLoop frameRate renderer tmap smap kbd tour etat (Just fin)
          Right (tour1, etat1) -> gameLoop frameRate renderer tmap smap kbd tour1 etat1 Nothing)
  putStrLn "bye bye"
-- (gameLoop frameRate renderer tmap smap kbd' gameState')
