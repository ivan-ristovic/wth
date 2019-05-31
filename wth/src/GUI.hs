module GUI where

import Model
import Logger as Log
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


guiWindow :: GG.Display
guiWindow = GG.InWindow "wth" (256, 256) (10, 10)

background :: G.Color
background = G.white

view :: Model -> IO G.Picture
view model = return $ G.pictures [getBackground model, getMap model, dotAt (getDotPos model)]

dotAt :: (Float, Float) -> G.Picture
dotAt p = 
    let x = fst p
        y = snd p
    in G.translate x y $ G.color G.red $ GG.circleSolid 3