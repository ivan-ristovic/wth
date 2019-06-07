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
formWeatherLayerUrl :: Layer -> Int -> Int -> Int -> IO String
formWeatherLayerUrl layer zoom tilex tiley = do
    token <- readFile "token.txt"
    let tok = Txt.unpack $ Txt.strip $ Txt.pack token
        layerStr = layerToStrInternal layer
        zStr = show zoom
        xStr = show tilex
        yStr = show tiley
        baseUrl = "https://tile.openweathermap.org/map/"
    return $ baseUrl ++ layerStr ++ "/" ++ zStr ++ "/" ++ xStr ++ "/" ++ yStr ++ ".png?appid=" ++ tok


formBackgroundMapUrl :: Int -> Int -> Int -> IO String
formBackgroundMapUrl zoom tilex tiley = 
    let zStr = show zoom
        xStr = show tilex
        yStr = show tiley
        baseUrl = "https://c.basemaps.cartocdn.com/light_all/"
     in return $ baseUrl ++ zStr ++ "/" ++ xStr ++ "/" ++ yStr ++ "@2x.png"


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
    return $ baseUrl ++ layerStr ++ "/" ++ zStr ++ "/" ++ xStr ++ "/" ++ yStr ++ "/?appid=" ++ token ++ ps


downloadImage :: String -> IO (Either String DynamicImage)
downloadImage mapUrl = do
    response <- openURI mapUrl
    case response of
        Left  err -> return (Left err)
        Right img -> return (decodePng img)


bgMapPath :: FilePath
bgMapPath = "res/worldmap.png"
