{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bool
import Options.Applicative
import Optparse
import Data.ByteString.Char8 (ByteString)
import Text.Regex.TDFA
import Data.Foldable
import qualified Data.ByteString.Char8 as B

default (ByteString)

main :: IO ()
main = do
  options <- execParser opts
  fileContents <- B.readFile $ filePath options
  B.putStrLn $ bool 
    ("not implimented") 
    (replaceColorVar $ fileContents) 
    (var options)

replaceColorVar :: ByteString -> ByteString
replaceColorVar fileContents = replaceAll pat colorToVar fileContents
  where 
    pat = makeRegex ("#[A-Za-z0-9]{6}" :: ByteString)

colorToVar :: ByteString -> ByteString
colorToVar color = "--" <> color <> ": " <> color

replaceAll :: Regex -> (ByteString -> ByteString) -> ByteString -> ByteString
replaceAll re f s = start end
  where (_, end, start) = foldl' go (0, s, id) $ getAllMatches (match re s :: AllMatches [] (Int, Int))
        go (ind,read,write) (off,len) =
            let (skip, start) = B.splitAt (off - ind) read 
                (matched, remaining) = B.splitAt len start 
            in (off + len, remaining, write . (skip <>) . (f matched <>))
