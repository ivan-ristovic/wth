module World where

import WeatherApi
import Codec.Picture.Types
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


data World = World
             { x    :: Float
             , y    :: Float
             , z    :: Int
             , l    :: Layer
             , bg   :: G.Picture
             , wmap :: G.Picture
             }
             

update :: Float -> World -> World
update _ w = w

changePos :: Float -> Float -> World -> World
changePos xNew yNew w = w { x = xNew, y = yNew }

changeZoom :: Int -> World -> World
changeZoom zNew w = w { z = zNew }

changeLayer :: Layer -> World -> World
changeLayer lNew w = w { l = lNew }

defaultWorld :: DynamicImage -> World
defaultWorld img =
    let pngMap = case GJ.fromDynamicImage img of Nothing  -> G.Blank
                                                 Just png -> png
    in World
      { x = 0
      , y = 0
      , z = 0
      , l = Temperature
      , bg = GG.png bgMapPath
      , wmap = pngMap
      }

test :: World
test = World { x = 0
             , y = 0
             , z = 0
             , l = Temperature
             , bg = GG.png bgMapPath
             , wmap = G.Blank
             }
