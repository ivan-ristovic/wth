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
             , getLayer
             , getMap
             , getBackground
             , updateModel
             , changeDotPos
             , changeZoom
             , changeLayer
             , changeMap
             , changeControls
             , addControl
             , getScreenSize
             , changeScreenSize
             , getWindowPosition
             ) where

import Codec.Picture.Types
import qualified WeatherApi as Api
import qualified Logger as Log
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ

data Control = Control
               { guid   :: Int
               , cx     :: Float
               , cy     :: Float
               , cw     :: Float
               , ch     :: Float
               , img    :: G.Picture
               , action :: Model -> IO Model
               }

data World = World
             { x    :: Float
             , y    :: Float
             , z    :: Int
             , l    :: Api.Layer
             , bg   :: G.Picture
             , wmap :: G.Picture
             , size :: ScreenSize
             , position :: WindowPosition
             }

type Model = (World, [Control])

type ScreenSize = (Int, Int)

type WindowPosition = (Int, Int)

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
               , size = (512, 512)
               , position = (100, 100)
               }
    in (world, [])

updateModel :: Float -> Model -> IO Model
updateModel _ model = do
    -- Log.debug "Updating model"
    -- Log.dbg $ show GA.getScreenSize
    return model

getScreenSize :: Model -> ScreenSize
getScreenSize model =
    let world = getWorld model
    in size world

getWindowPosition :: Model -> WindowPosition
getWindowPosition model =
    let world = getWorld model
    in position world

getDotPos :: Model -> (Float, Float)
getDotPos model =
    let world = getWorld model
    in (x world, y world)

getZoom :: Model -> Int
getZoom model =
    let world = getWorld model
    in z world

getLayer :: Model -> Api.Layer
getLayer model =
    let world = getWorld model
    in l world

getMap :: Model -> G.Picture
getMap model =
    let world = getWorld model
    in wmap world

getBackground :: Model -> G.Picture
getBackground model =
    let world = getWorld model
        sx = 1.0 / (8 + (fromIntegral $ z world));
        sy = sx
    in G.scale sx sy $ bg world

changeDotPos :: (Float, Float) -> Model -> Model
changeDotPos pos model =
    let world = getWorld model
    in changeWorld (world { x = fst pos, y = snd pos }) model

changeZoom :: Int -> Model -> Model
changeZoom zNew model =
    let world = getWorld model
    in changeWorld (world { z = if zNew < 0 then 0 else zNew }) model

changeLayer :: Api.Layer -> Model -> Model
changeLayer lNew model =
    let world = getWorld model
    in changeWorld (world { l = lNew }) model

changeMap :: DynamicImage -> Model -> Model
changeMap img model =
    let pngMap = case GJ.fromDynamicImage img of Nothing  -> G.Blank
                                                 Just png -> png
        world  = getWorld model
    in changeWorld (world { wmap = pngMap }) model

changeScreenSize :: (Int, Int) -> Model -> Model
changeScreenSize newSize model =
    let sx = fst newSize
        sy = snd newSize
        world = getWorld model
    in changeWorld (world { size = (sx, sy) }) model

changeControls :: [Control] -> Model -> Model
changeControls newControls model =
    let world = getWorld model
    in makeModel world newControls

addControl :: Control -> Model -> Model
addControl control model =
    let world      = getWorld model
        controls   = getControls model
        newControl = control { guid = getNextGuidInternal controls }
    in makeModel world (newControl : controls)


getNextGuidInternal :: [Control] -> Int
getNextGuidInternal controls = head $ dropWhile (\i -> any (\control -> guid control == i) controls) [1..]
