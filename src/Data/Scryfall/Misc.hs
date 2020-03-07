{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Data.Scryfall.Misc (CatalogF(..), ErrorF(..), ListF(..), RulingF(..)) where

import Data.Scryfall.Internal

import GHC.Generics

import Data.Text
import Data.Time
import Data.Vector
import Text.URI

data CatalogF f = Catalog
                { _catalogUri                 :: HKD f URI
                , _catalogTotalValues         :: HKD f Integer 
                , _catalogData                :: HKD f (Vector Text)
                } deriving (Generic)

data ErrorF f = Error
                { _errorStatus                :: HKD f Integer
                , _errorCode                  :: HKD f Text
                , _errorDetails               :: HKD f Text
                , _errorType                  :: HKD f Text
                , _errorWarnings              :: HKD f (Vector Text)
                } deriving (Generic)

data ListF f a = List
                { _listData                   :: HKD f (Vector a)
                , _listHasMore                :: HKD f Bool 
                , _listNextPage               :: HKD f (Maybe URI)
                , _listTotalCards             :: HKD f (Maybe Integer)
                , _listWarnings               :: HKD f (Vector Text)
                } deriving (Generic)

data RulingF f = Ruling
                { _rulingSource               :: HKD f Text --new data type?
                , _rulingPublishedAt          :: HKD f Day
                , _rulingComment              :: HKD f Text
                } deriving (Generic)
