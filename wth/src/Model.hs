module Model ( Component(..)
             , World(..)
             , Model(..)
             , defaultModel
             , getDotPos
             , getZoom
             , getMap
             , getBackground
             , updateModel
             , changeDotPos
             , changeZoom
             , changeLayer
             , changeMap
             ) where

import Codec.Picture.Types
import WeatherApi as Api
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


data Component = Component
                 { id   :: Int
                 , xpos :: Float
                 , ypos :: Float
                 , img  :: Float
                 }

data World = World
             { x    :: Float
             , y    :: Float
             , z    :: Int
             , l    :: Layer
             , bg   :: G.Picture
             , wmap :: G.Picture
             }

type Model = (World, [Component])


getWorldInternal :: Model -> World
getWorldInternal = fst

getComponentsInternal :: Model -> [Component]
getComponentsInternal = snd


defaultModel :: Model
defaultModel = 
    let world = World 
               { x = 0
               , y = 0
               , z = 0
               , l = Api.Temperature
               , bg = GG.png Api.bgMapPath
               , wmap = G.Blank
               }
    in (world, [])

updateModel :: Float -> Model -> IO Model
updateModel _ model = return model

getDotPos :: Model -> (Float, Float)
getDotPos model = 
    let world = getWorldInternal model
    in (x world, y world)

getZoom :: Model -> Int
getZoom model = 
    let world = getWorldInternal model
    in z world

getMap :: Model -> G.Picture
getMap model = 
    let world = getWorldInternal model
    in wmap world

getBackground :: Model -> G.Picture
getBackground model = 
    let world = getWorldInternal model
    in bg world

changeDotPos :: Float -> Float -> Model -> Model
changeDotPos xNew yNew model = 
    let world = getWorldInternal model 
    in (world { x = xNew, y = yNew }, getComponentsInternal model)

changeZoom :: Int -> Model -> Model
changeZoom zNew model = 
    let world = getWorldInternal model 
    in (world { z = zNew }, getComponentsInternal model)

changeLayer :: Layer -> Model -> Model
changeLayer lNew model = 
    let world = getWorldInternal model 
    in (world { l = lNew }, getComponentsInternal model)

changeMap :: DynamicImage -> Model -> Model
changeMap img model = 
    let pngMap = case GJ.fromDynamicImage img of Nothing  -> G.Blank
                                                 Just png -> png
        world = getWorldInternal model 
    in (world { wmap = pngMap }, getComponentsInternal model)
