module Parser
  ( gemtext
  ) where

import Data.Char

-- Lazily convert an input wikidata String to gemtext.
gemtext :: String -> String

gemtextInner :: State -> String -> String

-- Process the state in the context of the input string, returning the next
-- state, the parsed token, and the next unparsed portion of the input string.
-- It is invalid to call this with a Done state.
performState :: State -> String -> (Token, State, String)

gemtext = gemtextInner LineStart
gemtextInner s r = case performState s r of
                        (t, Done, _)       -> show t
                        (t, LineStart, rs) -> show t ++ "\n" ++ gemtextInner LineStart rs
                        (t, ss, rs)        -> show t ++ gemtextInner ss rs

data State = Done -- Input finished a token.
           | LineStart -- Input is starting a line.
           | ParBuild String -- Input is composing a paragraph.
           -- Input is opening a header with a given level.
           | HeaderOpen  Int
           -- Input is opening a header with a given level and text.
           | HeaderText  Int String
           -- Input is closing a header, with Int ='s left to go, of level Int,
           -- and with the given text.
           | HeaderClose Int Int String
           deriving Show

data Token = Paragraph String | Header Int String | HorizontalRule

-- `show` renders our token to gemtext
instance Show Token where
  show (Paragraph s) = s ++ "\n"

  show (Header n s) | n <= 3 = replicate n '#' ++ " " ++ s
   -- show headers of lesser importance in brackets
  show (Header _ s) = "### [" ++ s ++ "]"

  show HorizontalRule = "---"

performState LineStart ('=':s) = performState (HeaderOpen 1) s
performState LineStart ('-':'-':'-':'\n':s) = (HorizontalRule, LineStart, s)

performState LineStart (x:xs)  = performState (ParBuild [x]) xs

performState (ParBuild x) [] = (Paragraph x, Done, "")
performState (ParBuild x) ('\n':'\n':y) = (Paragraph x, LineStart, y)
performState (ParBuild x) (y:ys) = if isSpace y then -- collapse whitespace
                                         performState (ParBuild $ x ++ " ") ys
                                    else performState (ParBuild $ x ++ [y]) ys

performState (HeaderOpen n) ('=':s) = performState (HeaderOpen $ n + 1) s

-- skip openinig whitespace
performState (HeaderOpen n) (x:xs) | isSpace x = performState (HeaderText n "") xs
performState (HeaderOpen n) x = performState (HeaderText n "") x

-- If HeaderText ends prematurely, this is in fact *not* a header, but rather a
-- line that begins with some =='s. At this point, we reconstruct the text, and
-- back up with the knowledge that we're constructing a word.
performState (HeaderText n s) ('\n':xs) = let
             text = replicate n '=' ++ [' '] ++ s ++ ['\n'] ++ xs
             in performState (ParBuild "") text

performState (HeaderText n s) ('=':xs) = performState (HeaderClose (n - 1) (n - 1) s) xs
performState (HeaderText n s) (x:xs)   = performState (HeaderText n $ s ++ [x]) xs

performState (HeaderClose 0 x s) ('\n':y) = (Header (x + 1) s, LineStart, y)
performState (HeaderClose n x s) ('=':ys) = performState (HeaderClose (n - 1) x s) ys

-- If HeaderClose's line is terminated, this is in fact *not* a header, but
-- rather a line that looks something like `== abc =`. At this point, we
-- reconstruct the string (this can be done losslessly because whitespace is
-- unimportant) and resume scanning knowing that we are constructing a word.
performState (HeaderClose ending starting text) ('\n':ys) = let
             hText = replicate starting '=' ++ [' '] ++ text
                     ++ replicate (starting - ending) '=' ++ ['\n'] ++ ys
             in performState (ParBuild "") hText
