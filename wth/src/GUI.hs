module GUI where

import Model
import Painter
import Logger as Log
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as GG
import qualified Graphics.Gloss.Juicy as GJ

import Debug.Trace


windowSize :: (Int, Int)
windowSize = (256, 256)

windowPos :: (Int, Int)
windowPos = (10, 10)


background :: G.Color
background = G.white

view :: Model -> IO G.Picture
view model = 
    let controlImages    = map createControlPicture $ getControls model
        grid             = drawGrid windowSize (getZoom model + 2)
        backgroundImages = [getBackground model, getMap model, grid]  
    in return $ G.pictures (backgroundImages ++ controlImages ++ [drawPointerAt (getDotPos model)])


guiDisplay :: GG.Display
guiDisplay = GG.InWindow "wth" windowSize windowPos

guiCreateControls :: Model -> Model
guiCreateControls model = 
    let tmpbutton = Control 
                  { guid = 0
                  , cx = -20
                  , cy = -20
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
    in G.translate x y $ img control

    
buttonImgTempImpl :: G.Picture
buttonImgTempImpl = G.color G.red $ GG.rectangleSolid 50 50