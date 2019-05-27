module GUI where

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)


guiWindow :: String -> IO Window
guiWindow title = do
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False
    set window [windowTitle := title, containerBorderWidth := 100]
    onDestroy window mainQuit
    return window


guiLabelBox :: String -> IO HBox
guiLabelBox txt = do
    box   <- hBoxNew False 0
    set box [containerBorderWidth := 2]
    label <- labelNew (Just txt)
    boxPackStart box label PackNatural 3
    return box


guiButton :: String -> IO () -> IO Button
guiButton text callback = do
    button <- buttonNew
    buttonTitle <- guiLabelBox text
    containerAdd button buttonTitle
    onClicked button callback
    return button