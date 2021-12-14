module Parser
  ( gemtext
  ) where

import Data.Char
import Data.List

-- Lazily convert an input wikidata String to gemtext.
gemtext :: String -> String
gemtextInner :: State -> String -> String

gemtext = gemtextInner ParagraphStart
gemtextInner _ "" = ""
gemtextInner s x  =
  case performState s $ head x of
       Accept t s -> show t ++ gemtextInner s (tail x)
       nc -> gemtextInner nc (tail x)

-- Process the state in the context of the input string, returning the next
-- state, the parsed token, and the next unparsed portion of the input string.
-- It is invalid to call this with a Done state.
performState :: State -> Char -> State

data State = LineStart
           -- Input is starting a line.
           | ParagraphStart
           -- Input has started a paragraph.
           | WordStart
           -- Input is starting a word.
           | WordBuild String
           -- Input is building up a word.
           | HeaderOpen Int
           -- Input is opening a header with a given level.
           | HeaderText  Int String
           -- Input is opening a header with a given level and text.
           | HeaderClose Int Int String
           -- Input is closing a header, with Int ='s left to go, of level Int,
           -- and with the given text.
           | HorizontalRuleBuild Int
           -- Input has given us Int '-''s, and the input *might* be a
           -- horizontal rule.
           | Accept Token State
           -- Input was a complete token and accepted. If parsing should be
           -- resumed, the next state to use is given here.
           deriving Show

data Token = Word String | ParagraphBreak | Header Int String | HorizontalRule

-- `show` renders our token to gemtext
instance Show Token where
  show (Word s) = s ++ " "

  show ParagraphBreak = "\n\n"

  show (Header n s) | n <= 3 = replicate n '#' ++ " " ++ s ++ "\n"
   -- show headers of lesser importance in brackets
  show (Header _ s) = "### [" ++ s ++ "]" ++ "\n"

  show HorizontalRule = "---\n\n"

performState ParagraphStart '-'  = HorizontalRuleBuild 1
performState ParagraphStart '='  = HeaderOpen 1
performState ParagraphStart '\n' = ParagraphStart
performState ParagraphStart c    = WordBuild [c]

performState LineStart '\n' = Accept ParagraphBreak ParagraphStart
performState LineStart c    = WordBuild [c]

performState WordStart c = WordBuild [c]

performState (WordBuild x) '\n' = Accept (Word x) LineStart
performState (WordBuild x) c | isSpace c = Accept (Word x) WordStart
performState (WordBuild x) c = WordBuild $ x ++ [c]

performState (HeaderOpen n) '=' = HeaderOpen $ n + 1

-- skip opening whitespace
performState (HeaderOpen n) c | isSpace c = HeaderText n ""
performState (HeaderOpen n) c = HeaderText n [c]

-- If HeaderText ends prematurely, this is in fact *not* a header, but rather a
-- line that begins with some ='s. At this point, we reconstruct the text, and
-- back up with the knowledge that we're constructing a word.
performState (HeaderText n s) '\n'
  = WordBuild $ replicate n '=' ++ s ++ "\n"

performState (HeaderText n s) '=' = HeaderClose (n - 1) (n - 1) s
performState (HeaderText n s) x   = HeaderText n $ s ++ [x]

performState (HeaderClose 0 x s) '\n' = Accept (Header (x + 1) s) ParagraphStart
performState (HeaderClose n x s) '=' = HeaderClose (n - 1) x s

-- If HeaderClose's buffer doesn't end appropriately (none of the above
-- conditions were matched and a recursion isn't appropriate), this is in fact
-- *not* a header, but rather a line that looks something like `== abc =`. At
-- this point, we reconstruct the string (this can be done losslessly because
-- whitespace is unimportant) and resume scanning knowing that we are
-- constructing a word.
performState (HeaderClose ending starting text) c = let
             hText = replicate (starting + 1) '=' ++ text
                     ++ replicate (starting - ending + 1) '=' ++ [c]
             in WordBuild hText

performState (HorizontalRuleBuild n) '-' | n < 3 = HorizontalRuleBuild $ n + 1
performState (HorizontalRuleBuild 3) '\n' = Accept HorizontalRule ParagraphStart

-- If none of the above rules matched (the string isn't ---\n or a prefix
-- thereof), we need to back up and rebuild the token knowing that we're looking
-- at a paragraph.
performState (HorizontalRuleBuild n) c = WordBuild (replicate n '-' ++ [c])
