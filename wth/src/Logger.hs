module Logger (LogLevel (..), debug, logMessage) where
    
import Data.Time


data LogLevel = Debug
              | Info
              | Warning
              | Error
              | Critical


debug :: String -> IO ()
debug = logMessage Debug


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
    putStrLn ("[" ++ (show time) ++ "] [" ++ lvl ++ "] " ++ msg)