module GUI where

import Model
import Logger as Log
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


background :: G.Color
background = G.white

view :: Model -> IO G.Picture
view model = return $ G.pictures [getBackground model, getMap model, drawPointerAt (getDotPos model)]

drawPointerAt :: (Float, Float) -> G.Picture
drawPointerAt p = 
    let x = fst p
        y = snd p
    in G.translate x y $ G.color G.red $ GG.circleSolid 3


guiWindow :: GG.Display
guiWindow = GG.InWindow "wth" (256, 256) (10, 10)
