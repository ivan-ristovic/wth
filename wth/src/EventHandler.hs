module EventHandler where

import Model
import qualified Logger as Log
import qualified WeatherApi as Api
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ
import qualified Graphics.Gloss.Interface.Pure.Game as GPG


processEvent :: GG.Event -> Model -> IO Model
processEvent (GG.EventResize (x, y)) model = do
    let newModel = (getWorld model, [])
        newControls = map (\control -> control { cw = (fromIntegral x) / 10.0, ch = (fromIntegral y) / 10.0 }) $ snd model
     in return $ foldr addControl newModel newControls

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


processLayerChange :: Api.Layer -> (Int, Int) -> Model -> IO Model
processLayerChange layer size model = do
    let newModel = changeLayer layer model
     in downloadAndEditModel size newModel


processZoomChange :: (Int -> Int) -> Model -> IO Model
processZoomChange f model = do
    let oldZoom = getZoom model
     in return $ changeZoom (f oldZoom) model


downloadAndEditModel :: (Int, Int) -> Model -> IO Model
downloadAndEditModel size model =
    let dot = getDotPos model
        tilex = div (fst size) (floor (fst dot) + (fst size `div` 2 + 1))
        tiley = div (snd size) (floor (snd dot) + (snd size `div` 2 + 1))
        zoom = getZoom model
        layer = getLayer model
     in do
        url        <- Api.formApiUrl layer zoom tilex tiley
        Log.dbg $ "Downloading: " ++ url
        downloaded <- Api.downloadMap url
        Log.dbg "Download complete"
        case downloaded of Left err  -> do Log.err err
                                           return model
                           Right img -> return $ changeMap img model