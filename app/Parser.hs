module Parser
  ( gemtext
  ) where

import Data.Char

-- Lazily convert an input wikitext String to gemtext.
gemtext :: String -> String
gemtextInner :: State -> String -> String

gemtext = gemtextInner ParagraphStart
gemtextInner _ "" = ""
gemtextInner s x  =
  case trans s $ head x of
       (nc, Just t)  -> show t ++ gemtextInner nc (tail x)
       (nc, Nothing) -> gemtextInner nc (tail x)

-- Apply a character to state, returning the next state and
-- potentially a converted output String.
trans :: State -> Char -> (State, Maybe Token)

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

trans ParagraphStart '-'  = (HorizontalRuleBuild 1, Nothing)
trans ParagraphStart '='  = (HeaderOpen 1, Nothing)
trans ParagraphStart '\n' = (ParagraphStart, Nothing)
trans ParagraphStart '\'' = (PhraseStart 1, Nothing)
trans ParagraphStart c    = (PhraseBuild NoFmt [c], Nothing)

trans LineStart '\n' = (ParagraphStart, Just ParagraphBreak)
trans LineStart '\'' = (PhraseStart 1, Nothing)
trans LineStart c    = (PhraseBuild NoFmt [c], Nothing)

trans (PhraseStart n) '\'' = (PhraseStart (n + 1), Nothing)
trans (PhraseStart 2) c = (PhraseBuild Italic [c], Nothing)
trans (PhraseStart 3) c = (PhraseBuild Bold [c], Nothing)
trans (PhraseStart 5) c = (PhraseBuild BoldItalic [c], Nothing)
trans (PhraseStart n) c = (PhraseBuild NoFmt $ replicate n '\'' ++ [c], Nothing)

-- It looks like Wikipedia will automatically close unclosed formatting delims
-- at newlines (?). Replicate that here.
trans (PhraseBuild f x) '\n' = (LineStart, Just $ Word f x)

trans (PhraseBuild NoFmt x) '\'' = (PhraseBuild NoFmt $ x ++ "'", Nothing)
trans (PhraseBuild f x) '\'' = (PhraseClose (ticks f - 1) f $ x, Nothing)
trans (PhraseBuild f x) c = (PhraseBuild f $ x ++ [c], Nothing)

trans (PhraseClose 0 f x) '\n' = (LineStart, Just $ Word f x)
trans (PhraseClose 0 f x) c | isSpace c = (PhraseStart 0, Just $ Word f x)

trans (PhraseClose n f x) '\'' = (PhraseClose (n - 1) f $ x, Nothing)

-- We were gaslit, gatekept, and girlbossed. The input isn't actually closing
-- anything.
trans (PhraseClose n f x) c = let ns = PhraseBuild f $
                                    x ++ replicate ((ticks f) - n) '\'' ++ [c]
                              in (ns, Nothing)

trans (HeaderOpen n) '=' = (HeaderOpen $ n + 1, Nothing)

-- skip opening whitespace
trans (HeaderOpen n) c | isSpace c = (HeaderText n "", Nothing)
trans (HeaderOpen n) c = (HeaderText n [c], Nothing)

-- If HeaderText ends prematurely, this is in fact *not* a header, but rather a
-- line that begins with some ='s. At this point, we reconstruct the text, and
-- back up with the knowledge that we're constructing a word.
trans (HeaderText n s) '\n'
  = (PhraseBuild NoFmt $ replicate n '=' ++ s ++ "\n", Nothing)

trans (HeaderText n s) '=' = (HeaderClose (n - 1) (n - 1) s, Nothing)
trans (HeaderText n s) x   = (HeaderText n $ s ++ [x],       Nothing)

trans (HeaderClose 0 x s) '\n' = (ParagraphStart, Just $ Header (x + 1) s)
trans (HeaderClose n x s) '=' = (HeaderClose (n - 1) x s, Nothing)

-- If HeaderClose's buffer doesn't end appropriately (none of the above
-- conditions were matched and a recursion isn't appropriate), this is in fact
-- *not* a header, but rather a line that looks something like `== abc =`. At
-- this point, we reconstruct the string (this can be done losslessly because
-- whitespace is unimportant) and resume scanning knowing that we are
-- constructing a word.
trans (HeaderClose ending starting text) c = let
  hText = replicate (starting + 1) '=' ++ text
            ++ replicate (starting - ending + 1) '=' ++ [c]
          in (PhraseBuild NoFmt hText, Nothing)

trans (HorizontalRuleBuild n) '-' | n < 3 = (HorizontalRuleBuild $ n + 1, Nothing)
trans (HorizontalRuleBuild 3) '\n' = (ParagraphStart, Just HorizontalRule)

-- If none of the above rules matched (the string isn't ---\n or a prefix
-- thereof), we need to back up and rebuild the token knowing that we're looking
-- at a paragraph.
trans (HorizontalRuleBuild n) c =
  (PhraseBuild NoFmt (replicate n '-' ++ [c]), Nothing)

