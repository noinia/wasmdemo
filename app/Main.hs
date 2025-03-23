module Main (main) where

import qualified Data.ByteString as B
import           Data.Text (Text)
import           Text.XML.Expat.Tree


-- | Reads the data from a Bytestring into a proper Node
readXML :: B.ByteString -> Either XMLParseError (Node Text Text)
readXML = parse' defaultParseOptions

parseIpe :: IO ()
parseIpe = (readXML <$> B.readFile "pointInPolygon.ipe") >>= \case
  Left  err  -> putStrLn $ "error parsing" <> show err
  Right node -> putStrLn $ show node

main :: IO ()
main = do putStrLn "Hello, Haskell!"
          parseIpe
