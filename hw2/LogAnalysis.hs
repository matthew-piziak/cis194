{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Log

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map show) . (filter badEnough) . inOrder . build

badEnough :: LogMessage -> Bool
badEnough (LogMessage (Error s) _ _) = s > 50
badEnough _                          = False

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                   = []
inOrder (Node left node right) = inOrder left ++ [node] ++ inOrder right

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree              = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert mes Leaf                      = Node Leaf mes Leaf
insert mes@(LogMessage _ time _) (Node left node@(LogMessage _ nodeTime  _) right)
  | time <= nodeTime = Node (insert mes left) node right
  | otherwise = Node left node (insert mes right)

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

parseMessage :: String -> LogMessage
parseMessage ('I':rest) = LogMessage Info (timestamp rest) (message rest)
parseMessage ('W':rest) = LogMessage Warning (timestamp rest) (message rest)
parseMessage ('E':rest) = LogMessage (Error (severity s)) (timestamp rest'') (message rest'')
  where (s:rest') = words rest
        rest'' = unwords rest'
parseMessage other = Unknown other

timestamp :: String -> Int
timestamp = read . head . words

message :: String -> String
message = unwords . drop 1 . words

severity :: String -> Int
severity = read . head . words
