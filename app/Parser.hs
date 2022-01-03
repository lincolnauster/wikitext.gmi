module Parser
  ( gemtext
  ) where

import Data.Char

-- Lazily convert an input wikidata String to gemtext.
gemtext :: String -> String
gemtextInner :: State -> String -> String

gemtext = gemtextInner ParagraphStart
gemtextInner _ "" = ""
gemtextInner s x  =
  case trans s $ head x of
       Accept t s -> show t ++ gemtextInner s (tail x)
       nc -> gemtextInner nc (tail x)

-- Process the state in the context of the input string, returning the next
-- state, the parsed token, and the next unparsed portion of the input string.
-- It is invalid to call this with a Done state.
trans :: State -> Char -> State

data State = LineStart
           -- Input is starting a line.
           | ParagraphStart
           -- Input has started a paragraph.
           | PhraseStart Int
           -- Input is starting a phrase with Int preceding ticks. A phrase is a
           -- set of text with the same inline formatting (ex., ''an italic
           -- phrase'').
           | PhraseBuild Formatting String
           -- Input is building up a phrase (with given formatting).
           | PhraseClose Int Formatting String
           -- Input is closing a phrase with the assumed Formatting. It has
           -- Int ticks left to go before complete closure.
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

data Formatting = NoFmt | Bold | Italic | BoldItalic deriving Show

ticks :: Formatting -> Int

ticks NoFmt = 0
ticks Italic = 2
ticks Bold = 3
ticks BoldItalic = ticks Bold + ticks Italic

data Token = Word Formatting String
           | ParagraphBreak
           | Header Int String
           | HorizontalRule

-- `show` renders our token to gemtext
instance Show Token where
  show (Word NoFmt  s) = s ++ " "
  show (Word Italic s) = "*" ++ s ++ "* "
  show (Word Bold   s) = "**" ++ s ++ "** "
  show (Word BoldItalic s) = "**" ++ s ++ "** "

  show ParagraphBreak = "\n\n"

  show (Header n s) | n <= 3 = replicate n '#' ++ " " ++ s ++ "\n"
   -- show headers of lesser importance in brackets
  show (Header _ s) = "### [" ++ s ++ "]" ++ "\n"

  show HorizontalRule = "---\n\n"

trans ParagraphStart '-'  = HorizontalRuleBuild 1
trans ParagraphStart '='  = HeaderOpen 1
trans ParagraphStart '\n' = ParagraphStart
trans ParagraphStart '\'' = PhraseStart 1
trans ParagraphStart c    = PhraseBuild NoFmt [c]

trans LineStart '\n' = Accept ParagraphBreak ParagraphStart
trans LineStart '\'' = PhraseStart 1
trans LineStart c    = PhraseBuild NoFmt [c]

trans (PhraseStart n) '\'' = PhraseStart (n + 1)
trans (PhraseStart 2) c = PhraseBuild Italic [c]
trans (PhraseStart 3) c = PhraseBuild Bold [c]
trans (PhraseStart 5) c = PhraseBuild BoldItalic [c]
trans (PhraseStart n) c = PhraseBuild NoFmt $ replicate n '\'' ++ [c]

-- It looks like Wikipedia will automatically close unclosed formatting delims
-- at newlines (?). Replicate that here.
trans (PhraseBuild f x) '\n' = Accept (Word f x) LineStart

trans (PhraseBuild NoFmt x) '\'' = PhraseBuild NoFmt $ x ++ "'"
trans (PhraseBuild f x) '\'' = PhraseClose (ticks f - 1) f $ x
trans (PhraseBuild f x) c = PhraseBuild f $ x ++ [c]

trans (PhraseClose 0 f x) '\n' = Accept (Word f x) $ LineStart
trans (PhraseClose 0 f x) c | isSpace c = Accept (Word f x) $ PhraseStart 0

trans (PhraseClose n f x) '\'' = PhraseClose (n - 1) f $ x

-- We were gaslit, gatekept, and girlbossed. The input isn't actually closing
-- anything.
trans (PhraseClose n f x) c = PhraseBuild f $
                                  x ++ replicate ((ticks f) - n) '\'' ++ [c]

trans (HeaderOpen n) '=' = HeaderOpen $ n + 1

-- skip opening whitespace
trans (HeaderOpen n) c | isSpace c = HeaderText n ""
trans (HeaderOpen n) c = HeaderText n [c]

-- If HeaderText ends prematurely, this is in fact *not* a header, but rather a
-- line that begins with some ='s. At this point, we reconstruct the text, and
-- back up with the knowledge that we're constructing a word.
trans (HeaderText n s) '\n'
  = PhraseBuild NoFmt $ replicate n '=' ++ s ++ "\n"

trans (HeaderText n s) '=' = HeaderClose (n - 1) (n - 1) s
trans (HeaderText n s) x   = HeaderText n $ s ++ [x]

trans (HeaderClose 0 x s) '\n' = Accept (Header (x + 1) s) ParagraphStart
trans (HeaderClose n x s) '=' = HeaderClose (n - 1) x s

-- If HeaderClose's buffer doesn't end appropriately (none of the above
-- conditions were matched and a recursion isn't appropriate), this is in fact
-- *not* a header, but rather a line that looks something like `== abc =`. At
-- this point, we reconstruct the string (this can be done losslessly because
-- whitespace is unimportant) and resume scanning knowing that we are
-- constructing a word.
trans (HeaderClose ending starting text) c = let
  hText = replicate (starting + 1) '=' ++ text
            ++ replicate (starting - ending + 1) '=' ++ [c]
          in PhraseBuild NoFmt hText

trans (HorizontalRuleBuild n) '-' | n < 3 = HorizontalRuleBuild $ n + 1
trans (HorizontalRuleBuild 3) '\n' = Accept HorizontalRule ParagraphStart

-- If none of the above rules matched (the string isn't ---\n or a prefix
-- thereof), we need to back up and rebuild the token knowing that we're looking
-- at a paragraph.
trans (HorizontalRuleBuild n) c = PhraseBuild NoFmt (replicate n '-' ++ [c])
