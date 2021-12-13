module Parser
  ( gemtext
  ) where

import Data.Char
import Data.List

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
           -- Input is starting a line.
           | LineStart
           -- Input is composing a paragraph.
           | ParBuild String
           -- Input might be able to close a paragraph (with value String),
           -- having observed Int newlines.
           | ParClose String Int
           -- Input is opening a header with a given level.
           | HeaderOpen  Int
           -- Input is opening a header with a given level and text.
           | HeaderText  Int String
           -- Input is closing a header, with Int ='s left to go, of level Int,
           -- and with the given text.
           | HeaderClose Int Int String
           -- Input has given us Int '-''s, and the input *might* be a
           -- horizontal rule.
           | HorizontalRuleBuild Int
           deriving Show

data Token = Paragraph String | Header Int String | HorizontalRule

-- `show` renders our token to gemtext
instance Show Token where
  show (Paragraph s) = s ++ "\n"

  show (Header n s) | n <= 3 = replicate n '#' ++ " " ++ s
   -- show headers of lesser importance in brackets
  show (Header _ s) = "### [" ++ s ++ "]"

  show HorizontalRule = "---"

performState LineStart s | isPrefixOf "=" s
  = performState (HeaderOpen 1) $ tail s

performState LineStart s | isPrefixOf "-" s
  = performState (HorizontalRuleBuild 1) $ tail s

performState LineStart s = performState (ParBuild [head s]) $ tail s

performState (ParBuild x) "" = (Paragraph x, Done, "")
performState (ParBuild x) y | isPrefixOf "\n" y
  = performState (ParClose x 1) $ tail y

performState (ParBuild x) t = let y  = head t
                                  ys = tail t
                              in if isSpace y then -- collapse whitespace
                                      performState (ParBuild $ x ++ " ") ys
                                 else performState (ParBuild $ x ++ [y]) ys

performState (ParClose x n) s | head s == '\n' && n < 2
  = (Paragraph x, Done, tail s)

-- ParClose didn't see a paragraph delimiter, so just append to the paragraph
-- and keep building (or collapse whitespace).
performState (ParClose x _) s
  = performState (ParBuild $ x ++ [' ', head s]) $ tail s

performState (HeaderOpen n) s | isPrefixOf "=" s =
  performState (HeaderOpen $ n + 1) $ tail s

-- skip opening whitespace
performState (HeaderOpen n) x | isSpace $ head x =
  performState (HeaderText n "") $ tail x

performState (HeaderOpen n) x = performState (HeaderText n "") x

-- If HeaderText ends prematurely, this is in fact *not* a header, but rather a
-- line that begins with some ='s. At this point, we reconstruct the text, and
-- back up with the knowledge that we're constructing a word.
performState (HeaderText n s) x | head x == '\n' = let
             xs = tail x
             text = replicate n '=' ++ [' '] ++ s ++ ['\n'] ++ xs
             in performState (ParBuild "") text

performState (HeaderText n s) x | isPrefixOf "=" x
  = performState (HeaderClose (n - 1) (n - 1) s) $ tail x

performState (HeaderText n s) x =
  performState (HeaderText n $ s ++ [head x]) $ tail x

performState (HeaderClose 0 x s) y | isPrefixOf "\n" y
  = (Header (x + 1) s, LineStart, tail y)

performState (HeaderClose n x s) y | isPrefixOf "=" y
  = performState (HeaderClose (n - 1) x s) $ tail y

-- If HeaderClose's buffer doesn't end appropriately (none of the above
-- conditions were matched and a recursion isn't appropriate), this is in fact
-- *not* a header, but rather a line that looks something like `== abc =`. At
-- this point, we reconstruct the string (this can be done losslessly because
-- whitespace is unimportant) and resume scanning knowing that we are
-- constructing a word.
performState (HeaderClose ending starting text) y = let
             hText = replicate (starting + 1) '=' ++ " " ++ text
                     ++ replicate (starting - ending + 1) '=' ++ [head y]
             in performState (ParBuild hText) $ tail y

performState (HorizontalRuleBuild n) y | n < 3 && head y == '-'
  = performState (HorizontalRuleBuild $ n + 1) $ tail y

performState (HorizontalRuleBuild 3) y | head y == '\n'
  = (HorizontalRule, LineStart, tail y)

-- If none of the above rules matched (the string isn't ---\n or a prefix
-- thereof), we need to back up and rebuild the token knowing that we're looking
-- at a paragraph.
performState (HorizontalRuleBuild n) y
  = performState (ParBuild (replicate n '-')) y
