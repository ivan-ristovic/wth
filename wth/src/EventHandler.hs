module EventHandler where

import Model
import Codec.Picture.Types
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


processLayerChange :: Api.Layer -> Model -> IO Model
processLayerChange layer model = 
    let model' = changeLayer layer model
     in downloadAndEditLayerImage model'


processZoomIncrease :: Model -> IO Model
processZoomIncrease model = 
    let oldZoom = getZoom model
     in return $ changeZoom (oldZoom + 1) model


processZoomDecrease :: Model -> IO Model
processZoomDecrease model = 
    let oldZoom  = getZoom                      model
        model'   = changeZoom (oldZoom - 1)     model
        model''  = updateTileCoordinates (0, 0) model'
        model''' = changeApiZoom                model''
     in downloadAndEditLayerImage model'''


processZoomActivation :: Model -> IO Model
processZoomActivation model =
    let dot       = getDotPos model
        size      = getScreenSize model
        zoom      = getZoom model
        partx     = (fromIntegral (fst size)) / (2.0 ** (fromIntegral zoom))
        party     = (fromIntegral (snd size)) / (2.0 ** (fromIntegral zoom))
        movementX = (fromIntegral $ fst size) / 2 + (fst dot)
        movementY = (fromIntegral $ snd size) / 2 - (snd dot)
        tilex     = (floor (movementX / partx))
        tiley     = (floor (movementY / party))
        newModel  = updateTileCoordinates (tilex, tiley) model
        newModel' = changeApiZoom newModel
     in downloadAndEditLayerImage newModel'


downloadAndEditLayerImage :: Model -> IO Model
downloadAndEditLayerImage model =
    let az       = getApiZoom         model
        layer    = getLayer           model
        (tx, ty) = getTileCoordinates model
     in do
        layerUrl   <- Api.formWeatherLayerUrl (layer) az tx ty
        mapUrl     <- Api.formBackgroundMapUrl az tx ty
        Log.dbg $ "Downloading layer: " ++ layerUrl
        dlLayer    <- Api.downloadImage layerUrl
        Log.dbg $ "Downloading map: " ++ mapUrl
        dlBg       <- Api.downloadImage mapUrl
        Log.dbg "Download complete"
        processDownloadedImages model dlLayer dlBg


processDownloadedImages :: Model -> Either String DynamicImage -> Either String DynamicImage -> IO Model
processDownloadedImages model (Right layerImg) (Right bgImg) =
    return $ changeImages layerImg bgImg model 

processDownloadedImages model (Left err) _ = do Log.err err 
                                                return model
processDownloadedImages model _ (Left err) = do Log.err err 
                                                return model