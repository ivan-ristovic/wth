module Main where

import GUI
import Model
import WeatherApi as Api
import Logger as Log
import Codec.Picture.Types
import qualified Data.ByteString as BStr
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


glossWindow :: GG.Display
glossWindow = GG.InWindow "wth" (256, 256) (10, 10)

background :: G.Color
background = G.white

view :: Model -> G.Picture
view model = G.pictures [getBackground model, getMap model, dotAt (getDotPos model)]

dotAt :: (Float, Float) -> G.Picture
dotAt p = 
    let x = fst p
        y = snd p
    in G.translate x y $ G.color G.red $ GG.circleSolid 3

processEvent :: GG.Event -> Model -> Model
processEvent (GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (nx, ny)) model = changeDotPos nx ny model
processEvent _ w = w

-- downloadImageCallback :: IO ()
-- downloadImageCallback = do
--     url        <- Api.formApiUrl WindSpeed 0 0 0
--     downloaded <- Api.downloadMap url
--     Log.debug url
--     case downloaded of
--         Left err  -> putStrLn err
--         Right img -> GG.play glossWindow background 120 (W.defaultWorld img) (GG.scale 1 1 . view) processEvent [ W.update ]


main :: IO ()
main = GG.play glossWindow background 120 defaultModel view processEvent [ updateModel ]
