module Main where

import Lib
import Codec.Picture.Types
import Control.Monad.Trans(liftIO)
import qualified Data.ByteString as BStr
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Game as GG
import qualified Graphics.Gloss.Juicy as GJ
import qualified Graphics.UI.Gtk as GTK

-- window :: Display
-- window = InWindow "wth" (400, 400) (10, 10)

-- background :: Color
-- background = white

--drawing :: DynamicImage -> Float -> Picture
--drawing img _ = case fromDynamicImage img of 
--    Nothing  -> Blank
--    Just pngMap -> pictures [(png bgMapPath), pngMap]

main :: IO ()
main = do
--    url <- formApiUrl "temp_new" 0 0 0
--    downloaded <- downloadMap url
--    case downloaded of
--        Left err  -> putStrLn err
--        Right img -> animate window background (drawing img)
    GTK.initGUI
    window <- GTK.windowNew
    window `GTK.on` GTK.deleteEvent $ liftIO GTK.mainQuit >> return False
    GTK.widgetShowAll window
    GTK.mainGUI
