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

import Data.List
import Debug.Trace

glossWindow :: GG.Display
glossWindow = GG.InWindow "wth" (256, 256) (10, 10)

background :: G.Color
background = G.white

view :: World -> G.Picture
view w = G.pictures $ [bg w, wmap w, dotAt (x w) (y w)] ++ grid 1

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

grid :: Int -> [G.Picture]
grid zoom = map myDrawLine $ (mapHorizontal coordinates) ++ (mapVertical coordinates)
  where coordinates = calculateValues 5 5

myDrawLine :: [(Float, Float)] -> G.Picture
myDrawLine l = G.color G.red $ GG.line l

calculateValues :: Int -> Float -> [Float]
calculateValues n m = map (\x -> (-256.0 / 2) + x * (256.0 / m)) $ take (n - 1) [1.0..]

-- mapHorizontal :: [Float] -> [(Float, Float)]
mapHorizontal coords = map (\x -> [(-270 / 2, x), (270 / 2, x)]) coords

-- mapVertical :: [Float] -> [(Float, Float)]
mapVertical coords = map (\x -> [(x, 270.0 / 2.0), (x, -270.0 / 2.0)]) coords
