module GUI (guiWindow) where

import World as W
import WeatherApi as Api
import Logger as Log
import Codec.Picture.Types
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import qualified Data.ByteString as BStr
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ
import qualified Graphics.Gloss.Export.PNG as GEP
import qualified Graphics.UI.Gtk.Display.Image as GI

guiWindow :: IO Window
guiWindow = do
    window      <- guiFrame "WTH"
    image       <- guiImage "./res/blankmap.PNG"
    btnDownload <- guiButton "Download image" downloadImageCallback
    controls    <- guiControls
    windowBox   <- hBoxNew False 0
    containerAdd controls btnDownload
    set windowBox [containerBorderWidth := 2]
    containerAdd windowBox controls
    containerAdd windowBox image
    containerAdd window windowBox
    return window


guiFrame :: String -> IO Window
guiFrame title = do
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False
    set window [windowTitle := title, containerBorderWidth := 100]
    onDestroy window guiInternalWindowDestroyCallback
    Log.debug "[GUI] Created window"
    return window


guiLabelBox :: String -> IO HBox
guiLabelBox txt = do
    box   <- hBoxNew False 0
    set box [containerBorderWidth := 2]
    label <- labelNew (Just txt)
    boxPackStart box label PackNatural 3
    Log.debug $ "[GUI] Created label: " ++ txt
    return box


guiButton :: String -> IO () -> IO Button
guiButton txt callback = do
    button <- buttonNew
    buttonTitle <- guiLabelBox txt
    containerAdd button buttonTitle
    onClicked button callback
    Log.debug $ "[GUI] Created button: " ++ txt
    return button


guiInputBox :: String -> IO HBox
guiInputBox txt = do
  box <- hBoxNew False 0
  set box [containerBorderWidth := 2]
  label <- labelNew (Just txt)
  input <- entryNew
  containerAdd box label
  containerAdd box input
  return box


guiControls :: IO VBox
guiControls = do
  box <- vBoxNew False 0
  set box [containerBorderWidth := 2]
  xInput <- guiInputBox "x: "
  yInput <- guiInputBox "y: "
  adjust <- adjustmentNew 0 0 10 1 0 0
  slider <- hScaleNew adjust
  containerAdd box xInput
  containerAdd box yInput
  containerAdd box slider
  return box


guiImage :: String -> IO HBox
guiImage filePath = do
  box <- hBoxNew False 0
  set box [containerBorderWidth := 2]
  image <- imageNewFromFile filePath
  containerAdd box image
  return box


downloadImageCallback :: IO ()
downloadImageCallback = do
    url        <- Api.formApiUrl WindSpeed 0 0 0
    downloaded <- Api.downloadMap url
    Log.debug url
    case downloaded of
        Left err  -> putStrLn err
        Right img -> GEP.exportPictureToPNG (256, 256) background "./test_img.png"  $ view (W.defaultWorld img) --GG.play glossWindow background 120 (W.defaultWorld img) (GG.scale 1 1 . view) processEvent [ W.update ]


glossWindow :: GG.Display
glossWindow = GG.InWindow "wth" (256, 256) (10, 10)


background :: G.Color
background = G.white


view :: World -> G.Picture
view w = G.pictures $ [bg w, wmap w, dotAt (x w) (y w)] ++ grid 1


dotAt :: Float -> Float -> G.Picture
dotAt x y = G.translate x y $ G.color G.red $ GG.circleSolid 3


guiInternalWindowDestroyCallback :: IO()
guiInternalWindowDestroyCallback = do
    Log.debug "[GUI] Destroying window..."
    mainQuit


grid :: Int -> [G.Picture]
grid zoom = map myDrawLine $ (mapHorizontal coordinates) ++ (mapVertical coordinates) -- [[(-270.0 / 2.0, 0), (270.0 / 2.0, 0)], [(0, 270.0 / 2.0), (0, -270.0 / 2.0)]]
    where coordinates = calculateValues 11

myDrawLine :: [(Float, Float)] -> G.Picture
myDrawLine l = G.color G.red $ GG.line l

calculateValues :: Int -> [Float]
calculateValues n = map (\x -> (-256.0 / 2) + x * (256.0 / (fromIntegral n))) $ take (n - 1) [1.0..]

mapHorizontal :: [Float] -> [[(Float, Float)]]
mapHorizontal coords = map (\x -> [(-270.0 / 2.0, x), (270.0 / 2.0, x)]) coords

mapVertical :: [Float] -> [[(Float, Float)]]
mapVertical coords = map (\x -> [(x, 270.0 / 2.0), (x, -270.0 / 2.0)]) coords


-- processEvent :: GG.Event -> World -> World
-- processEvent (GG.EventKey (GG.MouseButton GG.LeftButton) GG.Down _ (nx, ny)) world =
--                   world { x = nx
--                         , y = ny
--                         }
-- processEvent _ w = w
