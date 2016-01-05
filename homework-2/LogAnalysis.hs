{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ('E':t) = parseError   t
parseMessage ('W':t) = parseWarning t
parseMessage ('I':t) = parseInfo    t
parseMessage message = Unknown message

parseError :: String -> LogMessage
parseError s = LogMessage (Error level) timeStamp message
  where level     = read $ (!! 0) $ words $ s
        timeStamp = read $ (!! 1) $ words $ s
        message   = unwords $ drop 2 $ words $ s

parseWarning :: String -> LogMessage
parseWarning s = LogMessage Warning timeStamp message
  where timeStamp = read $ head $ words $ s
        message   = unwords $ drop 2 $ words $ s

parseInfo :: String -> LogMessage
parseInfo s = LogMessage Info timeStamp message
  where timeStamp = read $ head $ words $ s
        message   = unwords $ drop 2 $ words $ s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert x@(LogMessage _ t1 _) (Node l y@(LogMessage _ t2 _) r) =
  if t1 > t2                    -- I'm not sure what this should do if
  then Node l y (insert x r)    -- the timestamps are equal, so I
  else Node (insert x l) y r    -- picked arbitrarily.
insert _ (Node _ (Unknown _) _) =
  error "Unknown message should not have been inserted into tree."

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l x r) = (inOrder l) ++ [x] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getMessage $ filter relevant sorted
  where relevant (LogMessage (Error n) _ _) = n >= 50
        relevant _ = False
        sorted = inOrder $ build messages
        getMessage (LogMessage _ _ message) = message
        getMessage (Unknown _) =
          error "Unknown message should not have been inserted into tree."
