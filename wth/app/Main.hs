module Main where

import GUI
import WeatherApi
import Logger as Log
import Codec.Picture.Types
import Graphics.UI.Gtk
import qualified Data.ByteString as BStr
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


window :: GG.Display
window = GG.InWindow "wth" (400, 400) (10, 10)

background :: G.Color
background = G.white

drawing :: DynamicImage -> Float -> G.Picture
drawing img _ = case GJ.fromDynamicImage img of 
    Nothing  -> G.Blank
    Just pngMap -> G.pictures [(GG.png bgMapPath), pngMap]

downloadImageCallback :: IO ()
downloadImageCallback = do
    url <- formApiUrl "temp_new" 0 0 0
    downloaded <- downloadMap url
    Log.debug url
    case downloaded of
        Left err  -> putStrLn err
        Right img -> G.animate window background (drawing img)

        
main :: IO ()
main = do
    initGUI
    window      <- guiWindow "WTH"
    btnDownload <- guiButton "Download image" downloadImageCallback
    containerAdd window btnDownload
    widgetShowAll window
    mainGUI
