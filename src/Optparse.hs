{-# LANGUAGE OverloadedStrings #-}

module Optparse where

import Data.Semigroup ((<>))
import Data.ByteString.Char8 (ByteString)
import Options.Applicative

default (ByteString)

data ColorOpt = ColorOpt
  { var        :: Bool 
  , name       :: Bool
  , filePath   :: FilePath
  , cmd        :: (Maybe String)
  }

opts :: ParserInfo ColorOpt
opts = info (colorOpt <**> helper)
  ( fullDesc
  <> progDesc "manipulate colors in a CSS/HTML file"
  )

colorOpt :: Parser ColorOpt
colorOpt = ColorOpt
      <$> switch
          ( long "var"
         <> short 'v'
         <> help "Convert colors to css variables"
          )
      <*> switch
          ( long "name"
         <> short 'n'
         <> help "name the variable of each color"
          )
      <*> strArgument
          ( help "Filepath of css/html file"
         <> metavar "PATH"
          )
      <*> option (optional str)
          ( help "Command to run on each color, formatted \"#XXXXXX\""
         <> long "command"
         <> short 'c'
         <> metavar "COMMAND"
          )

