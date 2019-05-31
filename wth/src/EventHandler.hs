module EventHandler where

import Model
import Logger as Log
import WeatherApi as Api
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


processEvent :: GG.Event -> Model -> IO Model

processEvent (GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (nx, ny)) model = return $ changeDotPos nx ny model

processEvent (GG.EventKey (GG.MouseButton GG.RightButton) GG.Down _ _) model = do
    url        <- Api.formApiUrl WindSpeed 0 0 0
    downloaded <- Api.downloadMap url
    Log.debug url
    case downloaded of
        Left err  -> return model
        Right img -> return $ changeMap img model

processEvent _ model = return model
