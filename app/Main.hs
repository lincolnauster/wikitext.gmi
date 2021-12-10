module Main where

import Data.Char

data State = Done -- Input finished a token.
           | LineStart -- Input is starting a line.
           | WordBuild String -- Input is composing a word.
           | HeaderOpen  Int        -- Input is opening a header with a given level.
           | HeaderText  Int String -- Input is opening a header with a given level and text.
           | HeaderClose Int Int String -- Input is closing a header, with Int ='s left to go, of level Int, and with the given text.
           deriving Show

data Token = Word String | Header Int String | HorizontalRule

-- `show` renders our token to gemtext
instance Show Token where
  show (Word s) = s ++ [' ']

  show (Header n s) | n <= 3 = replicate n '#' ++ " " ++ s
   -- show headers of lesser importance in brackets
  show (Header n s) = "### [" ++ s ++ "]"

  show (HorizontalRule) = "---"

-- Process the state in the context of the input string, returning the next
-- state, the parsed token, and the next unparsed portion of the input string.
-- It is invalid to call this with a Done state.
performState :: State -> String -> (Token, State, String)

performState LineStart ('=':s) = performState (HeaderOpen 1) s
performState LineStart ('-':'-':'-':'\n':s) = (HorizontalRule, LineStart, s)

performState LineStart (x:xs)  = performState (WordBuild [x]) xs

performState (WordBuild x) [] = (Word x, Done, "")
performState (WordBuild x) ('\n':y) = (Word x, LineStart, y)
performState (WordBuild x) (y:ys) | isSpace y = (Word x, WordBuild "", ys)
performState (WordBuild x) (y:ys) = performState (WordBuild $ x ++ [y]) ys

performState (HeaderOpen n) ('=':s) = performState (HeaderOpen $ n + 1) s
performState (HeaderOpen n) (x:xs) | isSpace x = performState (HeaderText n "") xs
performState (HeaderOpen n) x = performState (WordBuild $ replicate n '=') x

-- If HeaderText ends prematurely, this is in fact *not* a header, but rather a
-- line that begins with some =='s. At this point, we reconstruct the text, and
-- back up with the knowledge that we're constructing a word.
performState (HeaderText n s) ('\n':xs) = let
             text = replicate n '=' ++ [' '] ++ s ++ ['\n'] ++ xs
             in performState (WordBuild "") text

performState (HeaderText n s) (x:xs) = if isSpace x
                                       then case head xs of
                                            '=' -> performState (HeaderClose n (n - 1) s) xs
                                            _   -> performState (HeaderText n $ s ++ [x]) xs
                                       else performState (HeaderText n $ s ++ [x]) xs

performState (HeaderClose 0 x s) ('\n':y) = (Header (x + 1) s, LineStart, y)
performState (HeaderClose n x s) ('=':ys) = performState (HeaderClose (n - 1) x s) ys

-- If HeaderClose's line is terminated, this is in fact *not* a header, but
-- rather a line that looks something like `== abc =`. At this point, we
-- reconstruct the string (this can be done losslessly because whitespace is
-- unimportant) and resume scanning knowing that we are constructing a word.
performState (HeaderClose ending starting text) ('\n':ys) = let
             text = replicate starting '=' ++ [' '] ++ text
                    ++ replicate (starting - ending) '=' ++ ['\n'] ++ ys
             in performState (WordBuild "") text

gemtext      :: String -> String
gemtextInner :: State -> String -> String

gemtextInner s r = case performState s r of
                        (t, Done, _)      -> show t
                        (t, LineStart, r) -> show t ++ "\n" ++ gemtextInner LineStart r
                        (t, s, r)         -> show t ++ gemtextInner s r

gemtext s = gemtextInner LineStart s

main :: IO ()
main = do cont <- getContents
          putStrLn $ gemtext cont
