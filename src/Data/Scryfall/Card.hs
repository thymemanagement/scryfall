{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Data.Scryfall.Card (Legality(..), Game(..), CardFaceF(..), RelatedCardF(..), PreviewF(..), CardF(..)) where

import Data.Scryfall.Internal
import Data.Scryfall.Symbol

import Data.Functor.Identity
import GHC.Generics

import Data.HashMap.Strict
import Data.Text
import Data.Time
import Data.UUID
import Data.Vector
import Text.URI

data Legality = Legal | NotLegal | Restricted | Banned deriving (Generic,Eq,Ord,Read,Show)

data Game = Paper | MTGO | Arena deriving (Generic,Eq,Ord,Read,Show)

data CardFaceF f = CardFace
                { _cardFaceArtist          :: HKD f Text
                , _cardFaceColorIndicator  :: HKD f (Vector Color)
                , _cardFaceColors          :: HKD f (Vector Color)
                , _cardFaceFlavorText      :: HKD f Text
                , _cardFaceIllustrationId  :: HKD f (Maybe UUID)
                , _cardFaceImageUris       :: HKD f (HashMap Text Text)
                , _cardFaceLoyalty         :: HKD f Text
                , _cardFaceManaCost        :: HKD f Text
                , _cardFaceName            :: HKD f Text
                , _cardFaceOracleText      :: HKD f Text
                , _cardFacePower           :: HKD f Text
                , _cardFacePrintedName     :: HKD f Text
                , _cardFacePrintedText     :: HKD f Text
                , _cardFacePrintedTypeLine :: HKD f Text
                , _cardFaceToughness       :: HKD f Text
                , _cardFaceTypeLine        :: HKD f Text
                , _cardFaceWatermark       :: HKD f Text
                } deriving (Generic)

data RelatedCardF f = RelatedCard
               { _relatedCardId            :: HKD f UUID
               , _relatedCardComponent     :: HKD f Text
               , _relatedCardName          :: HKD f Text
               , _relatedCardTypeLine      :: HKD f Text
               , _relatedCardUri           :: HKD f URI
               } deriving (Generic)

data PreviewF f = Preview
               { _previewPreviewedAt       :: HKD f (Maybe Day)
               , _previewSourceUri         :: HKD f (Maybe URI)
               , _previewSource            :: HKD f (Maybe Text)
               } deriving (Generic)

type Preview = PreviewF Identity

data CardF f = Card
               --core fields
               { _cardArenaId              :: HKD f (Maybe Integer)
               , _cardId                   :: HKD f UUID
               , _cardLang                 :: HKD f Text
               , _cardMtgoId               :: HKD f (Maybe Integer)
               , _cardMtgoFoilId           :: HKD f (Maybe Integer)
               , _cardMultiverseIds        :: HKD f (Vector Integer)
               , _cardTcgplayerId          :: HKD f (Maybe Integer)
               , _cardOracleId             :: HKD f UUID
               , _cardPrintsSearchUri      :: HKD f URI
               , _cardRulingsUri           :: HKD f URI
               , _cardScryfallUri          :: HKD f URI
               , _cardUri                  :: HKD f URI
               -- gameplay fields
               , _cardAllParts             :: HKD f (Vector (RelatedCardF Identity))
               , _cardCardFaces            :: HKD f (Vector (CardFaceF Identity))
               , _cardCmc                  :: HKD f Double
               , _cardColors               :: HKD f (Vector Color)
               , _cardColorIdentity        :: HKD f (Vector Color)
               , _cardColorIndicator       :: HKD f (Vector Color)
               , _cardEdhrecRank           :: HKD f (Maybe Integer)
               , _cardFoil                 :: HKD f Bool
               , _cardHandModifier         :: HKD f Text
               , _cardLayout               :: HKD f Text
               , _cardLegalities           :: HKD f (HashMap Text Legality)
               , _cardLifeModifier         :: HKD f Text
               , _cardLoyalty              :: HKD f Text
               , _cardManaCost             :: HKD f Text
               , _cardName                 :: HKD f Text
               , _cardNonfoil              :: HKD f Bool
               , _cardOracleText           :: HKD f Text
               , _cardOversized            :: HKD f Bool
               , _cardPower                :: HKD f Text
               , _cardReserved             :: HKD f Bool
               , _cardToughness            :: HKD f Text
               , _cardTypeLine             :: HKD f Text
               -- print fields
               , _cardArtist               :: HKD f Text
               , _cardBooster              :: HKD f Bool
               , _cardBorderColor          :: HKD f Text
               , _cardCardBackId           :: HKD f UUID
               , _cardCollectorNumber      :: HKD f Text
               , _cardDigital              :: HKD f Bool
               , _cardFlavorText           :: HKD f Text
               , _cardFrameEffects         :: HKD f (Vector Text) --new data type?
               , _cardFrame                :: HKD f Text --new data type?
               , _cardFullArt              :: HKD f Bool
               , _cardGames                :: HKD f (Vector Game)
               , _cardHighresImage         :: HKD f Bool
               , _cardIllustrationId       :: HKD f (Maybe UUID)
               , _cardImageUris            :: HKD f (HashMap Text Text)
               , _cardPrices               :: HKD f (HashMap Text (Maybe Text))
               , _cardPrintedName          :: HKD f Text
               , _cardPrintedText          :: HKD f Text
               , _cardPrintedTypeLine      :: HKD f Text
               , _cardPromo                :: HKD f Bool
               , _cardPromoTypes           :: HKD f (Vector Text) --new data type?
               , _cardPurchaseUris         :: HKD f (HashMap Text URI)
               , _cardRarity               :: HKD f Text --new data type?
               , _cardRelatedUris          :: HKD f (HashMap Text URI)
               , _cardReleasedAt           :: HKD f Day
               , _cardReprint              :: HKD f Bool
               , _cardScryfallSetUri       :: HKD f URI
               , _cardSetName              :: HKD f Text
               , _cardSetSearchUri         :: HKD f URI
               , _cardSetType              :: HKD f Text
               , _cardSetUri               :: HKD f URI
               , _cardSet                  :: HKD f Text
               , _cardStorySpotlight       :: HKD f Bool
               , _cardTextless             :: HKD f Bool
               , _cardVariation            :: HKD f Bool
               , _cardVariationOf          :: HKD f (Maybe UUID)
               , _cardWatermark            :: HKD f Text
               , _cardPreview              :: HKD f (Maybe Preview)
               } deriving (Generic)
