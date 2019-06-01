module Logger (LogLevel (..), dbg, inf, wrn, err, crc, logMessage) where
    
import Data.Time


data LogLevel = Debug
              | Info
              | Warning
              | Error
              | Critical


dbg :: String -> IO ()
dbg = logMessage Debug

inf :: String -> IO ()
inf = logMessage Info

wrn :: String -> IO ()
wrn = logMessage Warning

err :: String -> IO ()
err = logMessage Error

crc :: String -> IO ()
crc = logMessage Critical


logMessage :: LogLevel -> String -> IO ()
logMessage lvl = case lvl of
    Debug    -> logMessageInternal "Debug"
    Info     -> logMessageInternal "Info"
    Warning  -> logMessageInternal "Warning"
    Error    -> logMessageInternal "Error"
    Critical -> logMessageInternal "Critical"


logMessageInternal :: String -> String -> IO ()
logMessageInternal lvl msg = do
    time <- getCurrentTime 
    putStrLn $ "[" ++ (show time) ++ "] [" ++ lvl ++ "] " ++ msg