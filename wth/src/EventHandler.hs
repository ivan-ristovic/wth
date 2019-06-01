module EventHandler where

import Model
import qualified Logger as Log
import qualified WeatherApi as Api
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


processEvent :: GG.Event -> Model -> IO Model
processEvent (GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (nx, ny)) model = do
    let world = getWorld model
        controls = getControls model
        isClickInsideControl = \ctl -> let x = cx ctl
                                           y = cy ctl
                                           w = cw ctl
                                           h = ch ctl
                                       in (nx >= x - w/2 && nx <= x + w/2) && (ny >= y - h/2 && ny <= y + h/2)
        activatedControls = filter isClickInsideControl controls
     in if null activatedControls then return $ changeDotPos (nx, ny) model
                                  else (action (head activatedControls)) model
processEvent _ model = return model


processLayerChange :: Api.Layer -> Model -> IO Model
processLayerChange layer model = do
    url        <- Api.formApiUrl layer 0 0 0
    Log.debug $ "Downloading: " ++ url
    downloaded <- Api.downloadMap url
    Log.debug "Download complete"
    let newModel = changeLayer layer model
     in case downloaded of Left err  -> do Log.logMessage Log.Error err
                                           return newModel
                           Right img -> return $ changeMap img newModel