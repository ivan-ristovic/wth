module Main where

import Lib
import Codec.Picture.Types
import Control.Monad.Trans(liftIO)
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

loadPepe :: IO ()
loadPepe = do
    url <- formApiUrl "temp_new" 0 0 0
    downloaded <- downloadMap url
    case downloaded of
        Left err  -> putStrLn err
        Right img -> G.animate window background (drawing img)

        
main :: IO ()
main = do
    initGUI
    w <- windowNew
--    w `on` deleteEvent $ liftIO mainQuit >> return False
    set w [windowTitle := "Pepe",
            containerBorderWidth := 100]
    button <- buttonNew
    onClicked button (loadPepe)
    box    <- labelBox "HONK!   "
    containerAdd button box
    containerAdd w button
    widgetShowAll w
    onDestroy w mainQuit
    mainGUI
    
labelBox :: String -> IO HBox
labelBox txt = do
  box   <- hBoxNew False 0
  set box [containerBorderWidth := 2]
  label <- labelNew (Just txt)
  boxPackStart box label PackNatural 3
  return box
