module Model ( Control(..)
             , World(..)
             , Model(..)
             , defaultModel
             , makeModel
             , getWorld
             , getControls
             , changeWorld
             , getDotPos
             , getZoom
             , getApiZoom
             , getLayer
             , getMap
             , getScreenSize
             , getWindowPosition
             , getTileCoordinates
             , updateModel
             , updateTileCoordinates
             , addControl
             , changeControls
             , changeDotPos
             , changeZoom
             , changeApiZoom
             , changeLayer
             , changeImages
             , changeScreenSize
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
             , apiZ :: Int
             , l    :: Api.Layer
             , tx   :: Int
             , ty   :: Int
             , picBg   :: G.Picture
             , picLayer :: G.Picture
             , size :: ScreenSize
             , position :: WindowPosition
             }

type Model = (World, [Control])
type ScreenSize = (Int, Int)
type WindowPosition = (Int, Int)
type TileCoordinates = (Int, Int)


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
               , apiZ = 0
               , l = Api.Temperature
               , tx = 0
               , ty = 0
               , picBg = GG.png Api.bgMapPath
               , picLayer = G.Blank
               , size = (512, 512)
               , position = (100, 100)
               }
    in makeModel world []

updateModel :: Float -> Model -> IO Model
updateModel _ model = return model

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

getApiZoom :: Model -> Int
getApiZoom model =
    let world = getWorld model
    in apiZ world

getLayer :: Model -> Api.Layer
getLayer model =
    let world = getWorld model
    in l world

getMap :: Model -> G.Picture
getMap model =
    let world          = getWorld model
        (sizex, sizey) = size world
        sx             = (/) (fromIntegral sizex) 256.0
        sy             = (/) (fromIntegral sizey) 256.0
        layerScaled    = G.scale sx sy $ picLayer world
        bgScaled       = picBg world --G.scale (sx / 2.0) (sy / 2.0) $ picBg world
    in G.pictures [bgScaled, layerScaled]

getTileCoordinates :: Model -> TileCoordinates
getTileCoordinates model =
    let world = getWorld model
    in (tx world, ty world)

updateTileCoordinates :: TileCoordinates -> Model -> Model
updateTileCoordinates (x, y) model =
    let world     = getWorld model
        zoom      = z world
        zPos t t' = t * round (2 ** (fromIntegral $ z world)) + t'
        zNeg t    = div t 2
        newTx     = if zoom >= 0 then zPos (tx world) x else zNeg (tx world)
        newTy     = if zoom >= 0 then zPos (ty world) y else zNeg (ty world)
    in changeWorld (world { tx = newTx , ty = newTy }) model

changeDotPos :: (Float, Float) -> Model -> Model
changeDotPos pos model =
    let world = getWorld model
    in changeWorld (world { x = fst pos, y = snd pos }) model

changeApiZoom :: Model -> Model
changeApiZoom model =
    let world = getWorld model
        zoom  = getZoom model
        aZ    = getApiZoom model
    in changeWorld (world { apiZ = max (aZ + zoom) 0, z = 0 }) model

changeZoom :: Int -> Model -> Model
changeZoom zNew model =
    let world = getWorld model
    in changeWorld (world { z = zNew }) model

changeLayer :: Api.Layer -> Model -> Model
changeLayer lNew model =
    let world = getWorld model
    in changeWorld (world { l = lNew }) model

changeImages :: DynamicImage -> DynamicImage -> Model -> Model
changeImages layerImg bgImg model =
    let layerPng = case GJ.fromDynamicImage layerImg of Nothing  -> G.Blank
                                                        Just png -> png
        bgPng    = case GJ.fromDynamicImage bgImg of Nothing  -> G.Blank
                                                     Just png -> png
        world  = getWorld model
    in changeWorld (world { picLayer = layerPng, picBg = bgPng }) model

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
