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


view :: Model -> IO G.Picture
view model =
    let controlImages    = map drawControl $ getControls model
        grid             = drawGrid (getScreenSize model) (2.0 ** (fromIntegral $ (max (getZoom model) 0)))
        backgroundImages = [getMap model, grid]
    in return $ G.pictures (backgroundImages ++ controlImages ++ [drawPointerAt (getDotPos model)])


guiDisplay :: Model -> GG.Display
guiDisplay model = GG.InWindow "wth" 
                               (getScreenSize model) (getWindowPosition model)


guiCreateControls :: Model -> Model
guiCreateControls model =
   let size    = getScreenSize model
       sizex   = (/) (fromIntegral $ fst size) 2.0
       sizey   = (/) (fromIntegral $ snd size) 2.0
       btnTemp = Control
                 { guid = 0
                 , cx = 15 - sizex
                 , cy = sizey - 15
                 , cw = 25
                 , ch = 25
                 , img = GGG.png "res/controls/temp.PNG"
                 , action = processLayerChange Api.Temperature
                 }
       btnWind = Control
                 { guid = 0
                 , cx = 45 - sizex
                 , cy = sizey - 15
                 , cw = 25
                 , ch = 25
                 , img = GGG.png "res/controls/wind.PNG"
                 , action = processLayerChange Api.WindSpeed
                 }
       btnPrec = Control
                 { guid = 0
                 , cx = 75 - sizex
                 , cy = sizey - 15
                 , cw = 25
                 , ch = 25
                 , img = GGG.png "res/controls/prec.PNG"
                 , action = processLayerChange Api.Precipitation
                 }
       btnClou = Control
                 { guid = 0
                 , cx = 105 - sizex
                 , cy = sizey - 15
                 , cw = 25
                 , ch = 25
                 , img = GGG.png "res/controls/clou.PNG"
                 , action = processLayerChange Api.Clouds
                 }
       btnPres = Control
                 { guid = 0
                 , cx = 135 - sizex
                 , cy = sizey - 15
                 , cw = 25
                 , ch = 25
                 , img = GGG.png "res/controls/pres.PNG"
                 , action = processLayerChange Api.Pressure
                 }
       btnZoomIn = Control
                   { guid = 0
                   , cx = sizex - 15
                   , cy = sizey - 15
                   , cw = 25
                   , ch = 25
                   , img = GGG.png "res/controls/zin.PNG"
                   , action = processZoomIncrease
                   }
       btnZoomOut = Control
                    { guid = 0
                    , cx = sizex - 15
                    , cy = sizey - 45
                    , cw = 25
                    , ch = 25
                    , img = GGG.png "res/controls/zout.PNG"
                    , action = processZoomDecrease
                    }
       btnZoom = Control
                 { guid = 0
                 , cx = sizex - 15
                 , cy = sizey - 75
                 , cw = 25
                 , ch = 25
                 , img = GGG.png "res/controls/enter.PNG"
                 , action = processZoomActivation
                 }
       allControls = [btnTemp, btnWind, btnPrec, btnClou, btnPres, btnZoomIn, btnZoomOut, btnZoom]
   in foldr addControl model allControls
