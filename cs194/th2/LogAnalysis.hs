{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseWords :: [String] -> LogMessage
parseWords ("I":tstamp:content) = LogMessage Info (read tstamp) (unwords content) 
parseWords ("W":tstamp:content) = LogMessage Warning (read tstamp) (unwords content)
parseWords ("E":severity:tstamp:content) = LogMessage (Error (read severity)) (read tstamp) (unwords content)
parseWords x = Unknown (unwords x)

parseMessage :: String -> LogMessage
parseMessage str = parseWords (words str)

parse :: String -> [LogMessage]
parse fullLog = [parseMessage line | line <- lines fullLog]

compareT :: LogMessage -> LogMessage -> Ordering
compareT (LogMessage _ a _) (LogMessage _ b _)  = compare a b

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMsg Leaf = Node Leaf newMsg Leaf
insert newMsg (Node left msg right) 
    | compareT newMsg msg == GT   = Node left msg (insert newMsg right) 
    | compareT newMsg msg == LT   = Node (insert newMsg left) msg right


buildWorker :: [LogMessage] -> MessageTree -> MessageTree
buildWorker [] tree = tree
buildWorker (x:xs) tree = insert x (buildWorker xs tree)   

build :: [LogMessage] -> MessageTree
build messages = buildWorker messages Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ inOrder(right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = 
    [show msg | msg@(LogMessage (Error sev) _ _) <- (inOrder $ build messages), sev >= 50]

