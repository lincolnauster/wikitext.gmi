module Main where

import System.IO
import System.Environment
import Parser

-- Given CLI args, locate and return a lazy string of the requested input.
targetStream :: IO String
targetStream = do a <- getArgs
                  case a of
                       ([])   -> getContents
                       (x:[]) -> readFile x

main :: IO ()
main = targetStream >>= \c -> putStrLn $ gemtext c
