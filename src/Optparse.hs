{-# LANGUAGE OverloadedStrings #-}

module Optparse where

import Data.Semigroup ((<>))
import Data.ByteString.Char8 (ByteString)
import Options.Applicative
import qualified Data.ByteString.Char8 as B

default (ByteString)

data ColorOpt = ColorOpt
  { var        :: Bool 
  , filePath   :: FilePath
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
      <*> strArgument
          ( help "Filepath of css/html file"
          )

