{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveGeneric, DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
module Data.Scryfall.Request where

import Data.Maybe
import Data.Proxy
import Data.Word
import GHC.Generics

import qualified Data.Text as T
import Lens.Micro
import qualified Text.URI as U
import Text.URI.QQ
import Text.URI.Lens

type PathPiece = U.RText U.PathPiece
type QueryKey = U.RText U.QueryKey
type QueryValue = U.RText U.QueryValue

class IsRequest r where
  requestUri :: r -> U.URI

class IsOption o where
  optionDefault :: o
  optionKey :: QueryKey
  optionValue :: o -> QueryValue

optionQuery :: forall o. (IsOption o, Eq o) => o -> [U.QueryParam]
optionQuery option
  | option == optionDefault = []
  | otherwise               = [U.QueryParam (optionKey @ o) (optionValue option)]

integerValue :: Word -> QueryValue
integerValue n = fromJust . U.mkQueryValue . T.pack . show $ n

integerPath :: Word -> PathPiece
integerPath n = fromJust . U.mkPathPiece . T.pack . show $ n

boolQuery :: QueryKey -> Bool -> [U.QueryParam]
boolQuery _ False = []
boolQuery key True = [U.QueryParam key [queryValue|true|]]

maybeQuery :: QueryKey -> Maybe (QueryValue) -> [U.QueryParam]
maybeQuery _ Nothing = []
maybeQuery key (Just value) = [U.QueryParam key value]

maybePath :: Maybe (PathPiece) -> [PathPiece]
maybePath Nothing = []
maybePath (Just path) = [path]

-- search queries (will be developed soon)

type SearchQuery = QueryValue

searchQuery :: SearchQuery -> U.QueryParam
searchQuery query = U.QueryParam [queryKey|q|] query

-- request options

newtype ListPage = ListPage { getPage :: Word }
  deriving (Eq,Ord,Read,Show,Generic)

instance IsOption ListPage where
  optionDefault = ListPage 0
  optionKey = [queryKey|page|]
  optionValue (ListPage n) = integerValue n
  

data ImageVersion = Small | Normal | Large | Png | ArtCrop | BorderCrop
  deriving (Eq,Ord,Read,Show,Generic)

instance IsOption ImageVersion where
  optionDefault = Large
  optionKey = [queryKey|version|]
  optionValue Small = [queryValue|small|]
  optionValue Normal = [queryValue|normal|]
  optionValue Large = [queryValue|large|]
  optionValue Png = [queryValue|png|]
  optionValue ArtCrop = [queryValue|art_crop|]
  optionValue BorderCrop = [queryValue|border_crop|]
    

data ImageFace = Front | Back
  deriving (Eq,Ord,Read,Show,Generic)

instance IsOption ImageFace where
  optionDefault = Front
  optionKey = [queryKey|face|]
  optionValue Front = [queryValue|front|]
  optionValue Back = [queryValue|back|]

data CardImageOptions = CardOptions { imageBack :: ImageFace, imageVersion :: ImageVersion }
  deriving (Eq,Ord,Show,Read,Generic)

cardImageQuery :: CardImageOptions -> [U.QueryParam]
cardImageQuery (CardOptions back version) = optionQuery back ++ optionQuery version

data UniqueOption = Cards | Art | Prints
  deriving (Eq,Ord,Show,Read,Generic)

instance IsOption UniqueOption where
  optionDefault = Cards
  optionKey = [queryKey|unique|]
  optionValue Cards = [queryValue|cards|]
  optionValue Art = [queryValue|art|]
  optionValue Prints = [queryValue|prints|]

data OrderOption = Name | Set | Released | Rarity | Color | Usd | Tix
                | Eur | Cmc | Power | Toughness | Edhrec | Artist
  deriving (Eq,Ord,Show,Read,Generic)


instance IsOption OrderOption where
  optionDefault = Name
  optionKey = [queryKey|order|]
  optionValue Name = [queryValue|name|]
  optionValue Set = [queryValue|set|]
  optionValue Released = [queryValue|released|]
  optionValue Rarity = [queryValue|rarity|]
  optionValue Color = [queryValue|color|]
  optionValue Usd = [queryValue|usd|]
  optionValue Tix = [queryValue|tix|]
  optionValue Eur = [queryValue|eur|]
  optionValue Cmc = [queryValue|cmc|]
  optionValue Power = [queryValue|power|]
  optionValue Toughness = [queryValue|toughness|]
  optionValue Edhrec = [queryValue|edhrec|]
  optionValue Artist = [queryValue|artist|]

    
data DirOption = Auto | Asc | Desc
  deriving (Eq,Ord,Read,Show,Generic)

instance IsOption DirOption where
  optionDefault = Auto
  optionKey = [queryKey|dir|]
  optionValue Auto = [queryValue|auto|]
  optionValue Asc = [queryValue|asc|]
  optionValue Desc = [queryValue|desc|]

    
data CardSearchOptions = CardSearchOptions
                         { uniqueOption :: UniqueOption
                         , orderOption :: OrderOption
                         , dirOption :: DirOption
                         , includeExtras :: Bool
                         , includeMultilingual :: Bool
                         , includeVariation :: Bool
                         } deriving (Eq,Ord,Read,Show,Generic)

searchOptionQuery :: CardSearchOptions -> [U.QueryParam]
searchOptionQuery (CardSearchOptions unique order dir extra multi var) =
  optionQuery unique ++ optionQuery order ++ optionQuery dir
  ++ boolQuery [queryKey|include_extras|] extra
  ++ boolQuery [queryKey|include_multilingual|] multi
  ++ boolQuery [queryKey|include_variation|] var

-- API Requests

data SetListRequest = Sets

instance IsRequest SetListRequest where
  requestUri Sets = [uri|/sets|]

data SetRequest = SetCode PathPiece
                | SetTcgPlayerId Word
                | SetId PathPiece

instance IsRequest SetRequest where
  requestUri (SetCode code) = [uri|/sets/|] & uriPath <>~ [code]
  requestUri (SetTcgPlayerId idVal) = [uri|/sets/tcgplayer/|] & uriPath <>~ [integerPath idVal]
  requestUri (SetId idVal) = [uri|/sets/|] & uriPath <>~ [idVal]

data CardListRequest = CardsList
                     | CardSearch SearchQuery CardSearchOptions

instance IsRequest CardListRequest where
  requestUri CardsList = [uri|/cards|]
  requestUri (CardSearch query options) = [uri|/cards/search|]
    & uriQuery <>~ [searchQuery query] ++ searchOptionQuery options

data CardRequest = CardNamedExact QueryValue (Maybe QueryValue)
                 | CardNamedFuzzy QueryValue (Maybe QueryValue)
                 | CardRandom (Maybe SearchQuery)
                 | CardCodeNumber PathPiece PathPiece (Maybe PathPiece)
                 | CardMultiverse Word
                 | CardMtgoId Word
                 | CardArenaId Word
                 | CardTcgPlayerId Word
                 | CardId PathPiece

instance IsRequest CardRequest where
  requestUri (CardNamedExact name set) = [uri|/cards/|]
    & uriQuery <>~ [U.QueryParam [queryKey|exact|] name] ++ maybeQuery [queryKey|set|] set
  requestUri (CardNamedFuzzy name set) = [uri|/cards/|]
    & uriQuery <>~ [U.QueryParam [queryKey|fuzzy|] name] ++ maybeQuery [queryKey|set|] set
  requestUri (CardRandom search) = [uri|cards/random|]
    & uriQuery <>~ maybeQuery [queryKey|q|] search
  requestUri (CardCodeNumber code num lang) = [uri|/cards/|]
    & uriPath <>~ [code,num] ++ maybePath lang
  requestUri (CardMultiverse idVal) = [uri|/cards/multiverse/|]
    & uriPath <>~ [integerPath idVal]
  requestUri (CardMtgoId idVal) = [uri|/cards/mtgo/|]
    & uriPath <>~ [integerPath idVal]
  requestUri (CardArenaId idVal) = [uri|/cards/arena/|]
    & uriPath <>~ [integerPath idVal]
  requestUri (CardTcgPlayerId idVal) = [uri|/cards/tcgplayer/|]
    & uriPath <>~ [integerPath idVal]
  requestUri (CardId idVal) = [uri|/cards/multiverse/|]
    & uriPath <>~ [idVal]

data CatalogRequest = CardAutoComplete QueryValue Bool
                    | CardNames
                    | ArtistNames
                    | WordBank
                    | CreatureTypes
                    | PlaneswalkerTypes
                    | LandTypes
                    | ArtifactTypes
                    | EnchantmentTypes
                    | SpellTypes
                    | Powers
                    | Toughnesses
                    | Loyalties
                    | Watermarks

instance IsRequest CatalogRequest where
  requestUri (CardAutoComplete name extras) = [uri|/cards/autocomplete|]
    & uriQuery <>~ [U.QueryParam [queryKey|q|] name] ++ boolQuery [queryKey|include_extras|] extras
  requestUri CardNames = [uri|/catalog/card-names|]
  requestUri ArtistNames = [uri|catalog/artist-names|]
  requestUri WordBank = [uri|/catalog/word-bank|]
  requestUri CreatureTypes = [uri|/catalog/creature-types/|]
  requestUri PlaneswalkerTypes = [uri|/catalog/planeswalker-types|]
  requestUri LandTypes = [uri|/catalog/land-types|]
  requestUri ArtifactTypes = [uri|/catalog/artifact-types|]
  requestUri EnchantmentTypes = [uri|/catalog/enchantment-types|]
  requestUri SpellTypes = [uri|/catalog/spell-types/|]
  requestUri Powers = [uri|/catalog/powers|]
  requestUri Toughnesses = [uri|/catalog/toughnesses|]
  requestUri Loyalties = [uri|/catalog/loyalties|]
  requestUri Watermarks = [uri|/catalog/watermarks|]
      
data RulingListRequest = RulingMultiverse Word
                   | RulingMtgoId Word
                   | RulingArenaId Word
                   | RulingCodeNumber PathPiece PathPiece
                   | RulingId PathPiece

rulingPath :: [PathPiece]
rulingPath = [[pathPiece|ruling|]]

instance IsRequest RulingListRequest where
  requestUri (RulingCodeNumber code num) = [uri|/cards/|]
    & uriPath <>~ [code,num] ++ rulingPath
  requestUri (RulingMultiverse idVal) = [uri|/cards/multiverse/|]
    & uriPath <>~ [integerPath idVal] ++ rulingPath
  requestUri (RulingMtgoId idVal) = [uri|/cards/mtgo/|]
    & uriPath <>~ [integerPath idVal] ++ rulingPath
  requestUri (RulingArenaId idVal) = [uri|/cards/arena/|]
    & uriPath <>~ [integerPath idVal] ++ rulingPath
  requestUri (RulingId idVal) = [uri|/cards/multiverse/|]
    & uriPath <>~ [idVal] ++ rulingPath

data SymbolListRequest = Symbology

instance IsRequest SymbolListRequest where
  requestUri Symbology = [uri|/symbology|]

data ManaRequest = ParseMana QueryValue

instance IsRequest ManaRequest where
  requestUri (ParseMana cost) = [uri|/symbology/parse-mana|]
    & uriQuery <>~ [U.QueryParam [queryKey|cost|] cost]

