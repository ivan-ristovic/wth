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
        grid             = drawGrid windowSize (getZoom model + 1)
        backgroundImages = [getBackground model, getMap model, grid]
    in return $ G.pictures (backgroundImages ++ controlImages ++ [drawPointerAt (getDotPos model)])


guiDisplay :: GG.Display
guiDisplay = GG.InWindow "wth" windowSize windowPos

guiCreateControls :: Model -> Model
guiCreateControls model =
    let btnTemp = Control
                  { guid = 0
                  , cx = -110
                  , cy = 110
                  , cw = 25
                  , ch = 25
                  , img = GGG.png "res/controls/temp.PNG"
                  , action = processLayerChange Api.Temperature windowSize
                  }
        btnWind = Control
                  { guid = 0
                  , cx = -80
                  , cy = 110
                  , cw = 25
                  , ch = 25
                  , img = GGG.png "res/controls/wind.PNG"
                  , action = processLayerChange Api.WindSpeed windowSize
                  }
        btnPrec = Control
                  { guid = 0
                  , cx = -50
                  , cy = 110
                  , cw = 25
                  , ch = 25
                  , img = GGG.png "res/controls/prec.PNG"
                  , action = processLayerChange Api.Precipitation windowSize
                  }
        btnClou = Control
                  { guid = 0
                  , cx = -20
                  , cy = 110
                  , cw = 25
                  , ch = 25
                  , img = GGG.png "res/controls/clou.PNG"
                  , action = processLayerChange Api.Clouds windowSize
                  }
        btnPres = Control
                  { guid = 0
                  , cx = 10
                  , cy = 110
                  , cw = 25
                  , ch = 25
                  , img = GGG.png "res/controls/pres.PNG"
                  , action = processLayerChange Api.Pressure windowSize
                  }
        btnZoomIn = Control
                    { guid = 0
                    , cx = 110
                    , cy = 110
                    , cw = 25
                    , ch = 25
                    , img = GGG.png "res/controls/zin.PNG"
                    , action = processZoomChange (+1)
                    }
        btnZoomOut = Control
                     { guid = 0
                     , cx = 110
                     , cy = 50
                     , cw = 25
                     , ch = 25
                     , img = GGG.png "res/controls/zout.PNG"
                     , action = processZoomChange (subtract 1)
                     }
        btnZoom = Control
                  { guid = 0
                  , cx = 110
                  , cy = 80
                  , cw = 25
                  , ch = 25
                  , img = GGG.png "res/controls/enter.PNG"
                  , action = downloadAndEditModel windowSize
                  }
        allControls = [btnTemp, btnWind, btnPrec, btnClou, btnPres, btnZoomIn, btnZoomOut, btnZoom]
    in foldr addControl model allControls
