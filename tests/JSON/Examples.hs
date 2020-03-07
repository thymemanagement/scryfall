{-# LANGUAGE TypeApplications, TemplateHaskell, OverloadedStrings #-}
module JSON.Examples (exampleTests) where

import Data.Scryfall
import JSON.TH

import Data.Text
import qualified Data.Vector as V
import Test.Tasty

generateTestParses exampleDir

exampleTests = testGroup "example tests"
               [ compareJSONTests badSearch badSearchValue
               , noCompareJSONTests (delver @Card)
               , noCompareJSONTests (ixalan @Set)
               , noCompareJSONTests (landTypes @Catalog)
               , noCompareJSONTests (ledRuling @(List Ruling))
               , noCompareJSONTests (notFound @Error)
               , noCompareJSONTests (parseMana @Mana)
               , noCompareJSONTests (progenitus @Card)
               , noCompareJSONTests (symbology @(List Symbol))
               , noCompareJSONTests (taoistHermit @Card)
               , noCompareJSONTests (wolves @(List Card))]

badSearchValue :: Error
badSearchValue = Error 400 "bad_request" "All of your terms were ignored." ""
  (V.fromList [ "Invalid expression “is:slick” was ignored. Checking if cards are “slick” is not supported"
              , "Invalid expression “cmc>cmc” was ignored. The sides of your comparison must be different."])

