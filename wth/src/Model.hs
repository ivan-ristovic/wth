module Model ( Control(..)
             , World(..)
             , Model(..)
             , defaultModel
             , getWorld
             , getControls
             , makeModel
             , changeWorld
             , getDotPos
             , getZoom
             , getMap
             , getBackground
             , updateModel
             , changeDotPos
             , changeZoom
             , changeLayer
             , changeMap
             , addControl
             ) where

import Codec.Picture.Types
import WeatherApi as Api
import qualified Logger as Log
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


data Control = Control
               { guid   :: Int
               , cx      :: Float
               , cy      :: Float
               , cw      :: Float
               , ch      :: Float
               , img    :: G.Picture
               , action :: World -> World
               }

data World = World
             { x    :: Float
             , y    :: Float
             , z    :: Int
             , l    :: Layer
             , bg   :: G.Picture
             , wmap :: G.Picture
             }

type Model = (World, [Control])


getWorld :: Model -> World
getWorld = fst

getControls :: Model -> [Control]
getControls = snd

makeModel :: World -> [Control] -> Model
makeModel world controls = (world, controls)

changeWorld :: World -> Model -> Model
changeWorld world model = makeModel world (getControls model)


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
updateModel _ model = do 
    -- Log.debug "Updating model"
    return model

getDotPos :: Model -> (Float, Float)
getDotPos model = 
    let world = getWorld model
    in (x world, y world)

getZoom :: Model -> Int
getZoom model = 
    let world = getWorld model
    in z world

getMap :: Model -> G.Picture
getMap model = 
    let world = getWorld model
    in wmap world

getBackground :: Model -> G.Picture
getBackground model = 
    let world = getWorld model
    in bg world

changeDotPos :: (Float, Float) -> Model -> Model
changeDotPos pos model = 
    let world = getWorld model 
    in changeWorld (world { x = fst pos, y = snd pos }) model

changeZoom :: Int -> Model -> Model
changeZoom zNew model = 
    let world = getWorld model 
    in changeWorld (world { z = zNew }) model

changeLayer :: Layer -> Model -> Model
changeLayer lNew model = 
    let world = getWorld model 
    in changeWorld (world { l = lNew }) model

changeMap :: DynamicImage -> Model -> Model
changeMap img model = 
    let pngMap = case GJ.fromDynamicImage img of Nothing  -> G.Blank
                                                 Just png -> png
        world  = getWorld model 
    in changeWorld (world { wmap = pngMap }) model

addControl :: Control -> Model -> Model
addControl control model = 
    let world      = getWorld model
        controls = getControls model
        newControl = control { guid = getNextGuidInternal controls }
    in makeModel world (newControl : controls)


getNextGuidInternal :: [Control] -> Int
getNextGuidInternal controls = head $ dropWhile (\i -> any (\control -> guid control == i) controls) [1..]