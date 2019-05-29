module Main where

import GUI
import Graphics.UI.Gtk


main :: IO ()
main = do
    initGUI
    window <- guiWindow
    widgetShowAll window
    mainGUI
