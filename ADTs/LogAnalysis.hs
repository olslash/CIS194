{-# OPTIONS_GHC -Wall #-}
module ADTs.LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = parse $ words s
    where parse (w:ws) = case w of "I" -> parseInfo ws
                                   "W" -> parseWarning ws
                                   "E" -> parseError ws
                                   _  -> Unknown "Unknown prefix"
          parse [] = Unknown "Empty log message"


parseInfo :: [String] -> LogMessage
parseInfo (time:msg) = LogMessage Info (read time :: TimeStamp) (unwords msg)
parseInfo _ = Unknown "Badly-formed info message"

parseWarning :: [String] -> LogMessage
parseWarning (time:msg) = LogMessage Warning (read time :: TimeStamp) (unwords msg)
parseWarning _ = Unknown "Badly-formed warning message"

parseError :: [String] -> LogMessage
parseError (code:time:msg) = LogMessage (Error (read code :: Int)) 
                                        (read time :: TimeStamp) 
                                        (unwords msg)
parseError _ = Unknown "Badly-formed error message"
