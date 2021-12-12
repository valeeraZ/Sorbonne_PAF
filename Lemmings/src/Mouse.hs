module Mouse where

import SDL
import Data.List (foldl')

type Mouse = Maybe (V2 Int)

handleEvent :: Event -> Mouse -> Mouse
handleEvent event _ =
  case eventPayload event of
    MouseButtonEvent me ->
      let P point = fmap fromIntegral (mouseButtonEventPos me) in Just point
    _ -> Nothing

-- | prise en compte des événements SDL2 pour mettre à jour l'état du souris
handleEvents :: [Event] -> Mouse -> Mouse
handleEvents events mouse = foldl' (flip handleEvent) mouse events
