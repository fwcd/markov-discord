module Config(
    botPrefix,
    mentionAuthor,
    respondToItself,
    chainLength
) where

botPrefix :: String
botPrefix = "&"

mentionAuthor :: Bool
mentionAuthor = False

respondToItself :: Bool
respondToItself = False

chainLength :: Int
chainLength = 40
