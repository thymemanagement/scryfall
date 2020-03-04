{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Data.Scryfall.Misc(Color(..), CatalogF(..), ErrorF(..), ListF(..), RulingF(..), SymbolF(..)) where

import Data.Scryfall.Internal

import GHC.Generics

import Data.Text
import Data.Time
import Data.Vector

data Color = White | Blue | Black | Red | Green deriving (Generic,Eq,Ord,Read,Show)

data CatalogF f a = Catalog
                { _catalogUri                 :: HKD f Text
                , _catalogTotalValues         :: HKD f Integer 
                , _catalogData                :: HKD f (Vector a)
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
                , _listNextPage               :: HKD f Text
                , _listTotalCards             :: HKD f (Maybe Integer)
                , _listWarnings               :: HKD f (Vector Text)
                } deriving (Generic)

data RulingF f = Ruling
                { _rulingSource               :: HKD f Text
                , _rulingPublishedAt          :: HKD f Day
                , _rulingComment              :: HKD f Text
                } deriving (Generic)

data SymbolF f = Symbol
                { _symbolSymbol               :: HKD f Text
                , _symbolLooseVariant         :: HKD f Text
                , _symbolEnglish              :: HKD f Text
                , _symbolTransposable         :: HKD f Bool
                , _symbolRepresentsMana       :: HKD f Bool
                , _symbolCmc                  :: HKD f (Maybe Double)
                , _symbolAppearsInManaCosts   :: HKD f Bool
                , _symbolFunny                :: HKD f Bool
                , _symbolColors               :: HKD f (Vector Color)
                , _symbolGathererAlternatives :: HKD f Text
                } deriving (Generic)
