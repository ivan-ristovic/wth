module Main where

import GUI
import EventHandler
import Model
import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO guiWindow 
              background 
              1 
              defaultModel 
              view 
              processEvent 
              updateModel
