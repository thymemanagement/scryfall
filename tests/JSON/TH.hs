{-# LANGUAGE DataKinds, TypeApplications, OverloadedStrings, TemplateHaskell, LambdaCase #-}
module JSON.TH ( exampleDir
               , TestParse
               , generateTestParses
               , parseSuccess
               , parseMatches
               , noCompareJSONTests
               , compareJSONTests) where

import Data.Scryfall
import Paths_scryfall

import Data.List
import Data.Traversable
import System.Directory
import System.IO

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Language.Haskell.TH
import Test.Tasty
import Test.Tasty.HUnit

exampleDir = "tests/examples/"

type TestParse a = (String, IO (Either String a))

fileParseName :: FilePath -> String
fileParseName filename = takeWhile (/='.') filename

parseTestJSON :: FromJSON a => FilePath -> FilePath -> TestParse a
parseTestJSON dirPath fileName = (fileParseName fileName, eitherDecodeFileStrict' (dirPath ++ fileName))

generateTestParses :: FilePath -> DecsQ
generateTestParses relDir = do
  dirPath <- runIO $ getDataFileName relDir
  files <- runIO $ listDirectory dirPath
  newVar <- newName "a"
  fmap concat $ forM files $ \filename -> do
    let parseName = mkName (fileParseName filename)
    sequence $ [ sigD parseName [t| (FromJSON $(varT newVar)) => TestParse $(varT newVar) |]
               , valD (varP parseName) (normalB [e| parseTestJSON $(stringE dirPath) $(stringE filename) |]) []]

parseSuccess :: (FromJSON a) => TestParse a -> TestTree
parseSuccess (fName, tParse) = testCase ("file parsed successfully") $ tParse >>=
  \ case
    Left str -> assertFailure str
    Right _ -> return ()

parseMatches :: (FromJSON a, Eq a, Show a) => TestParse a -> a -> TestTree
parseMatches (fName,tParse) eValue = testCase ("file matches expected value") $ tParse >>=
  \ case
    Left str -> assertFailure str
    Right x -> assertEqual "" eValue x

noCompareJSONTests :: (FromJSON a) => TestParse a -> TestTree
noCompareJSONTests (name,parse) = testGroup name [parseSuccess (name,parse)]

compareJSONTests :: (FromJSON a, Eq a, Show a) => TestParse a -> a -> TestTree
compareJSONTests (name,parse) eValue = testGroup name [parseSuccess (name,parse), parseMatches (name,parse) eValue]
