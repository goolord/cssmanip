{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Bool
import Data.ByteString.Char8 (ByteString)
import Data.Foldable
import Data.Functor.Identity
import Data.List (nub)
import Data.Map.Strict (Map, (!?))
import Data.Maybe
import Options.Applicative
import Optparse
import System.Process
import Text.Regex.TDFA
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  options <- execParser opts
  fileContents <- B.readFile $ filePath options
  when (var options)
       (B.putStrLn =<< (replaceColorVar fileContents $ name options))
  mapM_ (B.putStrLn =<<) 
        (apColor fileContents <$> cmd options)

apColor :: ByteString -> String -> IO ByteString
apColor fileContents cmd' = do 
  replaceAllListM matchColors runCmd fileContents
  where
    colors = nub $ getColors fileContents
    matchColors = map matchExact $ colors
    runCmd x = do 
      output <- readCreateProcess (shell cmd') (B.unpack x)
      pure $ B.pack output

replaceColorVar :: ByteString -> Bool -> IO ByteString
replaceColorVar fileContents nameB = do
  (varBlock, as) <- appendVar nameFun' colors
  let keyToVal x = "var(" <> ( B.takeWhile notColon $ fromMaybe mempty $ as !? x ) <> ")"
      nameFun  = bool (pure . variableColor) (pure . keyToVal) nameB
  (pure varBlock) <> (replaceAllListM matchColors nameFun fileContents)
  where 
    colors = nub $ getColors fileContents
    nameFun' = bool (pure . colorToVar) (colorInput colorToVarNamed) nameB
    matchColors = map matchExact $ colors
    notColon ':' = False
    notColon _ = True

matchExact :: ByteString -> Regex
matchExact b = makeRegex $ escapeREString b

colorInput :: (ByteString -> ByteString -> ByteString) -> ByteString -> IO ByteString
colorInput f color = do
  B.putStr $ color <> " name: "
  f <$> getLineNoMempty <*> pure color

getLineNoMempty :: IO ByteString
getLineNoMempty = do
  b <- B.getLine
  case b == mempty of
    True -> do B.putStr "Can't be empty: "
               getLineNoMempty
    False -> pure b

getColors :: ByteString -> [ByteString]
getColors content = 
     (getAllTextMatches $ content =~ pat1 :: [ByteString])
  <> (getAllTextMatches $ content =~ pat2 :: [ByteString])
  <> (getAllTextMatches $ content =~ pat3 :: [ByteString])
  where
    pat1 = "#[A-Za-z0-9]{6}" :: ByteString
    pat2 = "#[A-Za-z0-9]{3}\\>" :: ByteString
    pat3 = "rgba{0,1}\\([0-9]+, *[0-9]+, *[0-9]+(, *[0-9]+){0,1}\\)" :: ByteString

appendVar :: forall m. Monad m => (ByteString -> m ByteString) -> [ByteString] -> m (ByteString, Map ByteString ByteString)
appendVar nameFun content = do
  as <- (mapM nameFun content)
  varBlock <- do
    fmap 
      (\x -> (B.intercalate "\n" $ map (B.append "  " ) $ map (flip B.snoc ';') x))
      (pure as)
  let amap = M.fromList $ zip content as
  pure ( "html {\n" <> varBlock <> "\n}\n\n" 
       , amap )

colorToVar :: ByteString -> ByteString
colorToVar color = "--" <> color <> ": " <> color

colorToVarNamed :: ByteString -> ByteString -> ByteString
colorToVarNamed vname color = "--" <> vname <> ": " <> color

variableColor :: ByteString -> ByteString
variableColor color = "var(--" <> color <> ")"

variableColorNamed :: ByteString -> ByteString -> ByteString
variableColorNamed vname _ = "var(--" <> vname <> ")"

replaceAll :: Regex -> (ByteString -> ByteString) -> ByteString -> ByteString
replaceAll re f s = runIdentity $ replaceAllM re (Identity . f) s

replaceAllM :: Monad m => Regex -> (ByteString -> m ByteString) -> ByteString -> m ByteString
replaceAllM re f s = do
  (_, end, start) <- foldlM go (0, s, id) $ getAllMatches (match re s :: AllMatches [] (Int, Int))
  return (start end)
  where 
  go (ind,readr,write) (off,len) =
      let (skip, start) = B.splitAt (off - ind) readr
          (matched, remaining) = B.splitAt len start 
      in do new <- f matched
            return (off + len, remaining, write . (skip <>) . (new <>))

replaceAllList :: [Regex] -> (ByteString -> ByteString) -> ByteString -> ByteString
replaceAllList rs f s = runIdentity $ replaceAllListM rs (Identity . f) s

replaceAllListM :: Monad m => [Regex] -> (ByteString -> m ByteString) -> ByteString -> m ByteString
replaceAllListM (r:[]) f s = replaceAllM r f s
replaceAllListM (r:rs) f s = replaceAllListM rs f =<< replaceAllM r f s
replaceAllListM ([]) _ _ = pure mempty

escapeREString :: ByteString -> ByteString
escapeREString bs = B.foldr esc mempty bs
  where
    esc :: Char -> ByteString -> ByteString
    esc c t | isMetaChar c = '\\' `B.cons` c `B.cons` t
            | otherwise    = c `B.cons` t

-- | returns True iff the charactr is an RE meta character
-- ('[', '*', '{', etc.)
isMetaChar :: Char -> Bool
isMetaChar c = case c of
  '^'  -> True
  '\\' -> True
  '.'  -> True
  '|'  -> True
  '*'  -> True
  '?'  -> True
  '+'  -> True
  '('  -> True
  ')'  -> True
  '['  -> True
  ']'  -> True
  '{'  -> True
  '}'  -> True
  '$'  -> True
  _    -> False
