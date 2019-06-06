module EventHandler where

import Model
import qualified Logger as Log
import qualified WeatherApi as Api
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ
import Debug.Trace

processEvent :: GG.Event -> Model -> IO Model
-- processEvent (GG.EventResize (x, y)) model = -/-do
    -- let newModel = (getWorld model, [])
    --     newControls = map (\control -> control { cw = (fromIntegral x) / 10.0, ch = (fromIntegral y) / 10.0 }) $ snd model
    --  in return $ foldr addControl newModel newControls
    -- let controls = getControls model
    --     newModel = changeScreenSize (x, y) model
    --     oldSize  = getScreenSize model
    --     sx = (fromIntegral x) / (fromIntegral $ fst oldSize)
    --     sy = (fromIntegral y) / (fromIntegral $ snd oldSize)
    --     newControls =  --map (\c -> c { cx = sx * (cx c), cy = sy * (cy c) }) controls
    -- let world = getWorld $ changeScreenSize (x, y) model
        -- newModel = makeModel world []
     -- in return $ guiCreateControls newModel

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
     in downloadAndEditModel newModel


processZoomChange :: (Int -> Int) -> Model -> IO Model
processZoomChange f model = do
     Log.dbg $ "zoom: " ++ (show (getZoom model))
     Log.dbg $ "api zoom: " ++ (show (getApiZoom model))
     let oldZoom = getZoom model
      in return $ changeZoom (f oldZoom) model


processApiZoomChange :: Model -> IO Model
processApiZoomChange model = do
    let newModel = changeApiZoom model
     in downloadAndEditModel newModel


downloadAndEditModel :: Model -> IO Model
downloadAndEditModel model =
    let dot = getDotPos model
        size = getScreenSize model
        zoom = getApiZoom model
        partx = (fromIntegral (fst size)) / (2.0 ** (fromIntegral zoom))
        party = (fromIntegral (snd size)) / (2.0 ** (fromIntegral zoom))
        movementX = (fromIntegral $ fst size) / 2 + (fst dot)
        movementY = (fromIntegral $ snd size) / 2 - (snd dot)
        tilex = (floor (movementX / partx))
        tiley = (floor (movementY / party))
        layer = getLayer model
     in do
        Log.dbg $ "tilex: " ++ (show tilex)
        Log.dbg $ "tiley: " ++ (show tiley)
        Log.dbg $ "zoom: " ++ (show (getZoom model))
        Log.dbg $ "api zoom: " ++ (show (getApiZoom model))
        url        <- Api.formApiUrl layer zoom tilex tiley
        Log.dbg $ "Downloading: " ++ url
        downloaded <- Api.downloadMap url
        Log.dbg "Download complete"
        case downloaded of Left err  -> do Log.err err
                                           return model
                           Right img -> return $ changeMap img model
