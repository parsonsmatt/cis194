-- HW #2

{- OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise #1:

parse :: String -> [LogMessage]
parse str = map (parseMessage) (lines(str))
	

parseMessage :: String -> LogMessage
parseMessage str = LogMessage (getMessageType str)
								              (getTimeStamp str)
									            (getString str)

getMessageType :: String -> MessageType
getMessageType str
	| head (words str) == "I" = Info
	| head (words str) == "W" = Warning
	| head (words str) == "E" = Error (getErrorLevel str) 

getTimeStamp :: String-> TimeStamp
getTimeStamp str
	| isError (getMessageType str) = read (head (drop 2 (words(str)))) :: Int
	| otherwise = read (head (drop 1 (words (str)))) :: Int

getString :: String -> String
getString str
	| isError(getMessageType str) = "TODO: Unstub error."
	| otherwise = "TODO: Unstub I/W"

isError :: MessageType -> Bool
isError msg
	| msg == Info = False
	| msg == Warning = False
	| head (words (show msg)) == "Error" = True
  | otherwise = False

getErrorLevel :: String -> Int
getErrorLevel str = read (head (drop 1 (words str)))

