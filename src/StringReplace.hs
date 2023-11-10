module StringReplace where

-- | Replaces in a String all occurences of a substring with a replacement.
-- Example: replaceSubstring "Hello World, Hello World!" "Hello" "Hi" -> "Hi World, Hi World!"
replaceSubstring :: String -> String -> String -> String
replaceSubstring [] _ _ = []
replaceSubstring s find repl =
  if take (length find) s == find
    then repl ++ replaceSubstring (drop (length find) s) find repl
    else head s : replaceSubstring (tail s) find repl