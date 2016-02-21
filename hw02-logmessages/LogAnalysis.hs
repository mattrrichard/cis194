module LogAnalysis where

import Text.Read
import Log

parseFile :: String -> [LogMessage]
parseFile =
  map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage message =
  case parseMessageType message of
    Just (msgType, rest) ->
      case parseInt rest of
        (Just timestamp, msg) -> LogMessage msgType timestamp msg
        (Nothing, _) -> Unknown message
    Nothing ->
      Unknown message

parseInt :: String -> (Maybe Int, String)
parseInt =
  worker []
  where
    worker acc (x:xs)
      | x == ' ' =
        (readMaybe (reverse acc) :: Maybe Int, xs)

      | x `elem` ['0','1','2','3','4','5','6','7','8','9'] =
        worker (x : acc) xs

      | otherwise =
        (Nothing, xs)
    worker _ xs =
      (Nothing, xs)


parseMessageType :: String -> Maybe (MessageType, String)
parseMessageType ('I' : ' ' : rest) = Just (Info, rest)
parseMessageType ('W' : ' ' : rest) = Just (Warning, rest)
parseMessageType ('E' : ' ' : rest) =
  makeError (parseInt rest)
  where
    makeError (Just level, rest')
      | level <= 100 = Just (Error level, rest')
      | otherwise = Nothing
    makeError (Nothing, _) = Nothing
parseMessageType _ = Nothing


insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTime _) (Node left root@(LogMessage _ rootTime _) right) =
  if msgTime < rootTime then
    Node (insert msg left) root right
  else
    Node left root (insert msg right)
-- insert _ tree = tree
insert (Unknown _) tree = tree
insert _ tree@(Node _ (Unknown _) _) = tree


build :: [LogMessage] -> MessageTree
build =
  foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  case tree of
    Leaf -> []
    (Node left node right) ->
      let
        leftFlat = inOrder left
        rightFlat = inOrder right
      in
        leftFlat ++ (node : rightFlat)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
  map (\(LogMessage _ _ msg) -> msg)
  $ filter onlyErrors sortedMessages
  where
    sortedMessages =
      inOrder (build messages)

    onlyErrors (LogMessage (Error level) _ _ ) = level > 50
    onlyErrors _ = False
