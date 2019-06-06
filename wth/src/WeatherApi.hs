module WeatherApi where

import qualified Data.ByteString as BStr
import qualified Data.Text as Txt
import Network.Curl.Download
import Codec.Picture.Png
import Codec.Picture.Types

data Layer = Temperature
           | Clouds
           | Precipitation
           | Pressure
           | WindSpeed

layerToStrInternal :: Layer -> String
layerToStrInternal layer = case layer of Temperature   -> "temp_new"
                                         Clouds        -> "clouds_new"
                                         Precipitation -> "precipitation_new"
                                         Pressure      -> "pressure_new"
                                         WindSpeed     -> "wind_new"


-- https://openweathermap.org/api/weathermaps
formApiUrl :: Layer -> Int -> Int -> Int -> IO String
formApiUrl layer zoom tilex tiley = do
    token <- readFile "token.txt"
    let tok = Txt.unpack $ Txt.strip $ Txt.pack token
        layerStr = layerToStrInternal layer
        zStr = show zoom
        xStr = show tilex
        yStr = show tiley
        baseUrl = "https://tile.openweathermap.org/map/"
    return (baseUrl ++ layerStr ++ "/" ++ zStr ++ "/" ++ xStr ++ "/" ++ yStr ++ ".png?appid=" ++ tok)


-- https://openweathermap.org/api/weather-map-2  (prolly wont be used since we are poor)
formApiQuery :: Layer -> Int -> Int -> Int -> [(String, String)] -> IO String
formApiQuery layer zoom tilex tiley params = do
    token <- readFile "token2.txt"
    let layerStr = layerToStrInternal layer
        zStr = show zoom
        xStr = show tilex
        yStr = show tiley
        baseUrl = "https://maps.openweathermap.org/maps/2.0/weather/"
        ps = foldl (++) "" $ map (\p -> "&" ++ (fst p) ++ "=" ++ (snd p)) params
    return (baseUrl ++ layerStr ++ "/" ++ zStr ++ "/" ++ xStr ++ "/" ++ yStr ++ "/?appid=" ++ token ++ ps)


downloadMap :: String -> IO (Either String DynamicImage)
downloadMap mapUrl = do
    response <- openURI mapUrl
    case response of
        Left  err -> return (Left err)
        Right img -> return (decodePng img)


bgMapPath :: FilePath
bgMapPath = "res/worldmap.png"
