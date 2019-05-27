module WeatherApi ( formApiUrl, downloadMap, bgMapPath ) where

import qualified Data.ByteString as BStr
import Network.Curl.Download
import Codec.Picture.Png
import Codec.Picture.Types


-- https://openweathermap.org/api/weathermaps
formApiUrl :: String -> Int -> Int -> Int -> IO String
formApiUrl layer zoom tilex tiley = do 
    token <- readFile "token.txt"
    let zStr = show zoom
    let xStr = show tilex
    let yStr = show tilex
    let baseUrl = "https://tile.openweathermap.org/map/"
    return (baseUrl ++ layer ++ "/" ++ zStr ++ "/" ++ xStr ++ "/" ++ yStr ++ ".png?appid=" ++ token)


-- https://openweathermap.org/api/weather-map-2  (prolly wont be used since we are poor)
formApiQuery :: String -> Int -> Int -> Int -> [(String, String)] -> IO String
formApiQuery layer zoom tilex tiley params = do
    token <- readFile "token2.txt"
    let zStr = show zoom
    let xStr = show tilex
    let yStr = show tilex
    let baseUrl = "https://maps.openweathermap.org/maps/2.0/weather/"
    let ps = foldl (++) "" $ map (\p -> "&" ++ (fst p) ++ "=" ++ (snd p)) params 
    return (baseUrl ++ layer ++ "/" ++ zStr ++ "/" ++ xStr ++ "/" ++ yStr ++ "/?appid=" ++ token ++ ps)


downloadMap :: String -> IO (Either String DynamicImage)
downloadMap mapUrl = do
    response <- openURI mapUrl
    case response of 
        Left  err -> return (Left err)
        Right img -> return (decodePng img)


bgMapPath :: FilePath
bgMapPath = "res/pepe.png"