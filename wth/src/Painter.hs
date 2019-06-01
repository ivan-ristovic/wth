module Painter where

import Model
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as GG
import qualified Graphics.Gloss.Juicy as GJ


drawPointerAt :: (Float, Float) -> G.Picture
drawPointerAt p = 
    let x = fst p
        y = snd p
    in G.translate x y $ G.color G.red $ GG.circleSolid 3


drawGrid :: (Int, Int) -> Int -> G.Picture
drawGrid size n =
    let sizex = fromIntegral $ fst size
        sizey = fromIntegral $ snd size
        stepx = sizex / (fromIntegral n)
        stepy = sizex / (fromIntegral n)
        pointsx = map (\x -> x-(sizex/2.0)) [0.0, stepx .. sizex]
        pointsy = map (\x -> x-(sizey/2.0)) [0.0, stepy .. sizey]
        startx = cycle [0.0-(sizex/2.0)]
        starty = cycle [0.0-(sizey/2.0)]
        stopx = map (+sizex) startx
        stopy = map (+sizey) starty
        makePath = \p1 p2 -> [p1, p2]
        vertical   = zipWith (makePath) (zip pointsx startx) (zip pointsx stopx)
        horizontal = zipWith (makePath) (zip starty pointsy) (zip stopy pointsy)
    in G.pictures $ map (G.color G.blue . G.line) (horizontal ++ vertical)


drawControl :: Control -> G.Picture
drawControl control = 
    let x = cx control
        y = cy control
        w = cw control
        h = ch control
    in G.translate x y $ G.scale (w/50.0) (h/50.0) $ img control
