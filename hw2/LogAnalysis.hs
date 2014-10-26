-- HW #2

{- OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise #1:

parse :: String -> [LogMessage]
parse str = map (parseMessage) (lines(str))
	

parseMessage :: String -> LogMessage
parseMessage str = LogMessage msgType ts rest
                   where msgType = getMessageType str
                         ts      = getTimeStamp str
                         rest    = getRest str

getMessageType :: String -> MessageType
getMessageType str
	| firstWord == "I" = Info
	| firstWord == "W" = Warning
	| firstWord == "E" = Error (getErrorLevel str) 
	where firstWord = head (words str)

getTimeStamp :: String -> TimeStamp
getTimeStamp str
	| isError (getMessageType str) = read (head (drop 2 (words(str)))) :: Int
	| otherwise = read (head (drop 1 (words (str)))) :: Int

getRest :: String -> String
getRest str  
	| isError (getMessageType str) = unwords (drop 3 (words str))
	| otherwise                    = unwords (drop 2 (words str))

isError :: MessageType -> Bool
isError (Error _) = True
isError _         = False

getErrorLevel :: String -> Int
getErrorLevel = read . head . (drop 1) . words

-- Exercise #2: 

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf           = Node Leaf m Leaf
insert msg@(LogMessage _ ts _) tree@(Node left currMsg@(LogMessage _ currTs _) right)
	| ts > currTs = Node left currMsg (insert msg right)
	| ts < currTs = Node (insert msg left) currMsg right
	| otherwise   = tree

-- Exercise #3:

build :: [LogMessage] -> MessageTree
build []     = Leaf
build [msg]  = insert msg Leaf
build (m:ms) = insert m (build ms) 

-- Exercise #4:

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf m Leaf)   = [m]
inOrder (Node Leaf m rtree)  = [m] ++ inOrder rtree
inOrder (Node ltree m Leaf)  = inOrder ltree ++ [m]
inOrder (Node ltree m rtree) = inOrder ltree ++ [m] ++ inOrder rtree

-- Exercise #5:

-- List of Strings corresponding to LogMessages of type Error x where x > 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong [msg] 
	| passTest msg = [str]
	| otherwise    = []
	where str = getString msg
whatWentWrong (msg:msgs)
	| passTest msg = [str] ++ whatWentWrong msgs
	| otherwise    = [] ++ whatWentWrong msgs
	where str = getString msg

passTest :: LogMessage -> Bool
passTest (LogMessage (Error x) _ _)
	| x > 50    = True
	| otherwise = False
passTest _ = False

getString :: LogMessage -> String
getString (LogMessage _ _ str) = str
