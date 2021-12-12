module Main where

import System.IO
import System.Environment
import Text.Printf

import Parser

data Action = Help | Convert String

-- Given CLI args, locate and return a lazy string of the requested input.
getTargetAction :: IO Action
getTargetAction = do a <- getArgs
                     case a of
                          ("--help":[]) -> return Help
                          ([])   -> getContents >>= \c -> return $ Convert c
                          (f:[]) -> readFile f  >>= \c -> return $ Convert c

performAction :: Action -> IO ()
performAction Help = do
  pname <- getProgName
  putStrLn $ printf "Usage:\n\
\  %s [--help] | <input-file>\n\
\\n\
\  An input file may be supplied in the first CLI argument. If none is present,\n\
\  stdin is used." pname

performAction (Convert s) = putStrLn $ gemtext s

main :: IO ()
main = getTargetAction >>= performAction
