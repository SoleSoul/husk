{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.List.Split (splitOn, chunksOf, wordsBy)

greet :: Text -> Text
greet name = "Hello, " <> name <> "!"

main :: IO ()
main = do
  TIO.putStrLn (greet "Text")
  let t = "  The quick brown fox  "
  print (T.strip t, T.words t, T.length t)
  print (T.splitOn "," "a,b,,c", T.toUpper "shout", T.replace "o" "0" "foo boo")
  print (T.isPrefixOf "ab" "abc", T.breakOn "::" "a::b::c", T.intercalate "-" ["x", "y", "z"])
  print (map T.unpack (T.lines "one\ntwo"), T.pack (show (42 :: Int)), T.count "a" "banana")
  putStrLn "plain String literal still works"
  print (splitOn ", " "a, b, c", chunksOf 2 [1 .. 5 :: Int], wordsBy (== ';') ";a;;b;")
  print (read "\"quoted\"" :: Text, T.center 9 '*' "mid", T.justifyRight 5 '0' "42")
