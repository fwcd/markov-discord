module Config(
    botPrefix,
    mentionAuthor,
    respondToItself,
    chainLength
) where

-- The bot's command prefix in messages.
botPrefix :: String
botPrefix = "&"

-- Whether the response message should @ping the author.
mentionAuthor :: Bool
mentionAuthor = False

-- Whether the bot should respond to its own messages.
respondToItself :: Bool
respondToItself = False

-- The length of the Markov chain.
chainLength :: Int
chainLength = 40
