module GUI where

import Model
import Logger as Log
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


windowSize :: (Int, Int)
windowSize = (256, 256)

windowPos :: (Int, Int)
windowPos = (10, 10)


background :: G.Color
background = G.white

view :: Model -> IO G.Picture
view model = 
    let controlImages = map createControlPicture $ getControls model
        backgroundImages = [getBackground model, getMap model]  
    in return $ G.pictures (backgroundImages ++ controlImages ++ [drawPointerAt (getDotPos model)])

drawPointerAt :: (Float, Float) -> G.Picture
drawPointerAt p = 
    let x = fst p
        y = snd p
    in G.translate x y $ G.color G.red $ GG.circleSolid 3


guiDisplay :: GG.Display
guiDisplay = GG.InWindow "wth" windowSize windowPos

guiCreateControls :: Model -> Model
guiCreateControls model = 
    let tmpbutton = Control 
                  { guid = 0
                  , cx = 100
                  , cy = 100
                  , cw = 50
                  , ch = 50
                  , img = buttonImgTempImpl
                  , action = \world -> world { x = x world + 1 }
                  }
    in addControl tmpbutton model


createControlPicture :: Control -> G.Picture
createControlPicture control = 
    let x = cx control
        y = cy control
        x' = x - (fromIntegral $ fst windowSize) / 2.0
        y' = (fromIntegral $ snd windowSize) / 2.0 - y
    in G.translate x' y'  $ img control

    
buttonImgTempImpl :: G.Picture
buttonImgTempImpl = G.color G.red $ GG.rectangleSolid 50 50