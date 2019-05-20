module Main where

-- totally not stolen

import Lib
import qualified Data.ByteString as BStr
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture.Types


window :: Display
window = InWindow "wth" (400, 400) (10, 10)

background :: Color
background = black

drawing :: DynamicImage -> Float -> Picture
drawing img _ = case fromDynamicImage img of 
    Nothing -> Blank
    Just png -> pictures [png]

main :: IO ()
--main = animate window background drawing
--main = formApiUrl "temp_new" 0 0 0 >>= putStrLn
main = do
    url <- formApiUrl "temp_new" 0 0 0
    downloaded <- downloadMap url
    case downloaded of
        Left  err -> putStrLn err
        Right img -> animate window background (drawing img)