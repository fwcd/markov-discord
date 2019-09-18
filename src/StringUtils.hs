module StringUtils(
    textReplace
) where

import qualified Data.Text as T

textReplace :: String -> String -> T.Text -> T.Text
textReplace pat repl = (T.replace (T.pack pat) (T.pack repl))
