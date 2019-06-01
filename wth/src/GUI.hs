module GUI where
    
import Model
import EventHandler
import Painter
import qualified Logger as Log
import qualified WeatherApi as Api
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GGG
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
    let controlImages    = map drawControl $ getControls model
        grid             = drawGrid windowSize (getZoom model + 2)
        backgroundImages = [getBackground model, getMap model, grid]  
    in return $ G.pictures (backgroundImages ++ controlImages ++ [drawPointerAt (getDotPos model)])


guiDisplay :: GG.Display
guiDisplay = GG.InWindow "wth" windowSize windowPos

guiCreateControls :: Model -> Model
guiCreateControls model = 
    let tmpbutton = Control 
                  { guid = 0
                  , cx = -110
                  , cy = 110
                  , cw = 25
                  , ch = 25
                  , img = GGG.png "res/controls/temp.PNG"
                  , action = processLayerChange Api.Temperature
                  }
    in addControl tmpbutton model