module Main where

import GUI
import EventHandler
import Model
import Painter
import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO (guiDisplay defaultModel)
              backgroundColor
              1
              (guiCreateControls defaultModel)
              view
              processEvent
              updateModel
