{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage msg = case head (words msg) of
    "I" -> LogMessage Info second (unwords (drop 2 line))
    "W" -> LogMessage Warning second (unwords (drop 2 line))
    "E" -> LogMessage (Error second) (read third :: Int) (unwords (drop 3 line))
    _   -> Unknown msg
    where line = words msg
          second = read (line !! 1) :: Int
          third = line !! 2


parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)




insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ toInsert _) tree = case tree of 
    Leaf -> Node Leaf msg Leaf
    Node left (LogMessage _ ts _) right -> 
        if (toInsert < ts) then insert msg left
            else insert msg right
    _ -> tree
insert (Unknown _) tree = tree
