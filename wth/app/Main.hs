module Main where

import GUI
import World as W
import WeatherApi as Api
import Logger as Log
import Codec.Picture.Types
import Graphics.UI.Gtk
import qualified Data.ByteString as BStr
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


glossWindow :: GG.Display
glossWindow = GG.InWindow "wth" (256, 256) (10, 10)

background :: G.Color
background = G.white

view :: World -> G.Picture
view w = G.pictures [bg w, wmap w, dotAt (x w) (y w)]

dotAt :: Float -> Float -> G.Picture
dotAt x y = G.translate x y $ G.color G.red $ GG.circleSolid 3

processEvent :: GG.Event -> World -> World
processEvent (GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (nx, ny)) world =
                  world { x = nx
                        , y = ny
                        }
processEvent _ w = w

downloadImageCallback :: IO ()
downloadImageCallback = do
    url        <- Api.formApiUrl WindSpeed 0 0 0
    downloaded <- Api.downloadMap url
    Log.debug url
    case downloaded of
        Left err  -> putStrLn err
        Right img -> GG.play glossWindow background 120 (W.defaultWorld img) (GG.scale 1 1 . view) processEvent [ W.update ]


main :: IO ()
main = do
    initGUI
    window      <- guiWindow "WTH"
    btnDownload <- guiButton "Download image" downloadImageCallback
    containerAdd window btnDownload
    widgetShowAll window
    mainGUI
