module Main where

import GUI
import EventHandler
import Model
import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO guiDisplay 
              background 
              1 
              (guiCreateControls defaultModel)
              view 
              processEvent 
              updateModel
