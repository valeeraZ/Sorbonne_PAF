# Lemmings Projet de PAF

ZHAO Wenzhuo: 3971004

HOU Zhen: 3970968

# Description

En jouant au jeu vidéo "Lemmings", nous nous sommes inspirés de copier ce jeu avec le langage Haskell avec le support graphique SDL. Le jeu s'avance à la vitesse 1 fps qui est relativement lente pour que le joueur puisse réfléchir et faire des opérations dans un instant assez long. Le joueur peut sélectionner un lemming et lui affecter une classe à la souris et au clavier, qui peut être une classe dans la liste ci-dessous:

- marcheur 
- tombeur 
- poseur,
- stoppeur
- creuseur

# Type

Avec les types utilisés dans le partiel, nous avons aussi des autres classes:

- `Coord`: la coordonnée d'un objet, soit un lemming, soit un bloc des case
- `Environnement`: la relation entre la coordonnée d'un objet et son status
- `Niveau`: une présentation des cases sur le jeu
- `Etat`: l'état du jeu qui représente l'environnement et le niveau sur un tour
- `Moteur`: traitement du changement d'état dans des tours de jeu, soit la fin du jeu, soit la continuation du jeu
- `Mouse` et `Keyboard`: traitement des événements du clavier et de la souris
- `Sprite`: affichage d'images sur une surface
- `SpriteMap`: une `map` de `Sprite` 
- `TextureMap`: chargement de fichiers des images

# Propriétés

## Coordonné
Se déplacer à gauche et à droite et monter et descendre de la même distance reviendra aux coordonnées d'origine
1. prop_bougeCoordGaucheDroite
2. prop_bougeCoordGaucheHaut
	
## Niveau
Le terrain de jeu doit être entouré d'un cercle de grilles métalliques.
Il ne peut y avoir qu'une seule entrée et une seule sortie dans le jeu.
Le case supérieur de l'entrée doit être vide.
Le case au-dessus de la sortie doit être en métal.
Chaque niveau ne peut pas dépasser la portée du champ de jeu.

1. prop_niveauFerme
2. prop_niveauEntreeSortie
3. prop_niveauEntreeCorrecte
4. prop_niveauSortieCorrecte
5. prop_niveauInclusion
6. prop_niveauInvariant
	
## Environnement
L'identifiant de Lemming se trouve dans le MAP de l'environnement, et peut également être trouvé dans l'identifiant de la séquence.
Après avoir modifié le lemming, il apparaîtra dans la liste et la MAP. Avant de supprimer le lemming, le lemming existe dans la liste et la MAP.

1. prop_enviInclusion1
2. prop_enviInclusion2
3. prop_envi_inv
4. prop_pre_enleve
5. prop_post_enleve
6. prop_post_deplace
7. prop_post_addEntite


## Etat
Pour être vivant, le nombre de sommes ou de lemmings restants doit être supérieur ou égal à 0.
1. prop_etat

## Lemming
Les coordonnées du lemming mort sont correctes. Lemming se déplace de la même distance à gauche et à droite pour revenir à la bonne position.
1. prop_tueLemming_post
2. prop_bougeLemmingDHDBG

# Tests

## EnvSpec.hs : 
tester les propriétés de l'environnement 
1. Environnement initiale
2. Add entite
3. deplacement entite
4. pre_enleve
5. post_enleve

## EtatSpec : 
tester les propriétés de l'etat
1. Etat initiale

## NiveauSpec : 
tester les propriétés du niveau
1. Cases comprises dans la niveau
2. Une entree dans le cordonne correcte
3. Une sortie dans le cordonne correcte
4. Unique entree/sortie
5. Teste tous

## QuickCheckEntite : 
Nous avons créé un générateur d'entités pour tester s'il s'agit d'un Entite correct et confirmer s'il s'agit d'une forme correcte de lemming. Le générateur a une probabilité de 80% de générer un Creuseur et une probabilité de 20% de générer un MarcheurG.
1. Entite générateur QuickCheck invariant

## QuickCheckNiveau :
Nous avons créé un générateur de Niveau pour générer un Niveau de manière aléatoire. On teste si Niveau est une forme correcte en utilisant les propriétés de Niveau.
1. Teste l'invariant pour les niveaux générées aléatoirement

# Manuel d'utilisation

1. Veuillez assurer que vous disposez de l'environnement `stack`

2. Installer la librairie `SDL`:

   1. Installer `SDL2` (TME 6)
   2. Installer `sdl2-ttf`: Pour OS X, vous pouvez utiliser la commande `brew install sdl2_ttf`; Pour Linux, vous pouvez utiliser la commande `sudo apt-get install libsdl-ttf2.0-dev` (sans avoir testé, veuillez en lire plus sur https://www.libsdl.org/projects/SDL_ttf/)

3. Utiliser `stack build` pour compiler et `stack run` pour démarrer le jeu

4. Une fois le jeu est démarré, vous visualisez cet écran:

    ![Screenshot 2021-05-21 at 19.08.21](https://dev.azure.com/zslyvain/9285f0e6-8055-4a5c-aec3-50d9555ac078/_apis/git/repositories/4eb461c6-bb1f-489f-978b-686e8c32decf/items?path=%2F1639343970259_3414.png&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0)

    Appuyez sur "S" en clavier pour commencer.

5. ![Screenshot 2021-05-21 at 19.08.58](https://dev.azure.com/zslyvain/9285f0e6-8055-4a5c-aec3-50d9555ac078/_apis/git/repositories/4eb461c6-bb1f-489f-978b-686e8c32decf/items?path=%2F1639343987248_951.png&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0)

   Comme la description du jeu original, le but du jeu est de faire le plus de lemmings à arriver à la sorite en bas de carte.

6. ![](https://dev.azure.com/zslyvain/9285f0e6-8055-4a5c-aec3-50d9555ac078/_apis/git/repositories/4eb461c6-bb1f-489f-978b-686e8c32decf/items?path=%2F1639344001222_5690.png&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0)

   Choisissez un lemming en cliquant long temps sur son affichage, un réticule s'affiche qui vous indique le lemming sélectionné. Selon le texte à droite, vous pouvez appuyer sur M/C/P/S pour lui affecter une classe. Le nombre de lemmings restants à entrer, de lemming vivants et de lemmings sauvés sont aussi affichés en haut à droite. Voici des effets possibles:

   ![Screenshot 2021-05-07 at 14.48.19](https://dev.azure.com/zslyvain/9285f0e6-8055-4a5c-aec3-50d9555ac078/_apis/git/repositories/4eb461c6-bb1f-489f-978b-686e8c32decf/items?path=%2F1639344015090_7837.png&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0)

7. Si vous avez sauvé au moins une moitié de lemmings, vous visualisez la "Victoire" sinon vous allez voir la "Défaite"

   ![Screenshot 2021-05-21 at 20.51.40](https://dev.azure.com/zslyvain/9285f0e6-8055-4a5c-aec3-50d9555ac078/_apis/git/repositories/4eb461c6-bb1f-489f-978b-686e8c32decf/items?path=%2F1639344028294_6335.png&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0)

   ​		![Screenshot 2021-05-21 at 19.09.49](https://dev.azure.com/zslyvain/9285f0e6-8055-4a5c-aec3-50d9555ac078/_apis/git/repositories/4eb461c6-bb1f-489f-978b-686e8c32decf/items?path=%2F1639344042191_593.png&versionDescriptor%5BversionOptions%5D=0&versionDescriptor%5BversionType%5D=0&versionDescriptor%5Bversion%5D=master&resolveLfs=true&%24format=octetStream&api-version=5.0)

8. Pendant tout au long du jeu, vous pouvez appuyer sur "Échappe" sur le clavier pour quitter le jeu.

# Développement

## Utilisation d'algébriques

Le jeu ne se déroule pas tout seul, il est accompagné avec l'état. En langage fonctionnel on ne conserver pas d'une variable qui est "static" comme C/Java. Passer une variable `Etat` ne semble pas assez élégant, donc nous utilisons `State Monad`  sur le type `Etat` qui permet de conserver l'état et le résultat en même temps. Les primitives de `State Monad` permet de modifier et conserver l'état du jeu entre des tours.

## Chargement et affichage d'images

Dans le `Main.hs`, la fonction `main` est bien typé de `IO ()` qui permet d'afficher des images avec l'aide de SDL2. Il existe de nombreux fichiers et de nombreux images à charger dans `SpriteMap` et `TextureMap`. Le chargement va utiliser et mettre à jour les `SpriteMap` et `TextureMap`, donc il est nécessaire d'introduire `Monad` pour avoir l'accès au status précédent des variables et passer le nouvelle mise à jour des variables à l'étape suivante. Soit une liste de fichiers à charger:

```haskell
-- liste d'éléments (Texture ID, FilePath) à charger
let elementsList = [("lemming-d", "assets/lemming-droite.bmp"), ("lemming-g", "assets/lemming-gauche.bmp"), 
										("poseur-d", "assets/poseur-droite.bmp"), ("poseur-g", "assets/poseur-gauche.bmp"),
									  ]
```

Nous implémentons une fonction `loadElement` qui prend les variables `SpriteMap` et `TextureMap` en entrée et donner leurs nouveaux mises à jours en `IO ()`:

```haskell
loadElement :: Renderer -> FilePath -> String -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
```

En langage impératif, on peut utiliser la boucle pour "itérer" la liste et mettre à jour les `SpriteMap` et `TextureMap` en tout moment. Cela est une combinaison de `fold` et `monad`, qui est possible à réaliser avec `foldM`:

```haskell
-- chargement de l'image du fond
(tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
-- chargement des images
(tmap, smap) <- foldM (\(t,s) (id, path) -> loadElement renderer path id t s ) (tmap, smap) elementsList
```

## Affichage de texte

La librairie SDL2 ne supporte pas d'afficher le texte, mais la librairie "SDL2-ttf" qui est compatible avec SDL2 offre des telles APIs. En s'inspirant du type `Sprite`, on arrive à "rendre" un texte sur une "surface" puis copier cette surface à `Renderer`. Il est bien aussi possible de paramétrer la taille de police et la position du texte.

## Difficultés

La librairie "SDL2" offre des APIs qui écoutent les événements de clavier et de souris, mais pour le programme, il est diffcile à comprendre la différence entre l'action "appuyer" et "cliquer". Il est possible que le programme ignore l'action d'utilisateurs, donc rend le jeu vidéo moins intéressant.