module GUI (guiWindow, guiLabelBox, guiButton) where

import Logger as Log
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)

import qualified Graphics.Gloss.Game as Game

guiWindow :: String -> IO Window
guiWindow title = do
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


guiInternalWindowDestroyCallback :: IO()
guiInternalWindowDestroyCallback = do
    Log.debug "[GUI] Destroying window..."
    mainQuit
