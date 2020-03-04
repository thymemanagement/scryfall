{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Data.Scryfall.Card (Legality(..), Game(..), CardFaceF(..), RelatedCardF(..), CardF(..)) where

import Data.Scryfall.Internal
import Data.Scryfall.Misc

import Data.Functor.Identity
import GHC.Generics

import Data.HashMap.Strict
import Data.Text
import Data.Time
import Data.Vector

data Legality = Legal | NotLegal | Restricted | Banned deriving (Generic,Eq,Ord,Read,Show)

data Game = Paper | MTGO | Arena deriving (Generic,Eq,Ord,Read,Show)

data CardFaceF f = CardFace
                { _cardFaceArtist          :: HKD f Text
                , _cardFaceColorIndicator  :: HKD f (Vector Color)
                , _cardFaceColors          :: HKD f (Vector Color)
                , _cardFaceFlavorText      :: HKD f Text
                , _cardFaceIllustrationId  :: HKD f Text
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
               { _relatedCardId            :: HKD f Text
               , _relatedCardComponent     :: HKD f Text
               , _relatedCardName          :: HKD f Text
               , _relatedCardTypeLine      :: HKD f Text
               , _relatedCardUri           :: HKD f Text
               } deriving (Generic)

data CardF f = Card
               --core fields
               { _cardArenaId              :: HKD f (Maybe Integer)
               , _cardId                   :: HKD f Text
               , _cardLang                 :: HKD f Text
               , _cardMtgoId               :: HKD f (Maybe Integer)
               , _cardMtgoFoilId           :: HKD f (Maybe Integer)
               , _cardMultiverseIds        :: HKD f (Vector Integer)
               , _cardTcgplayerId          :: HKD f (Maybe Integer)
               , _cardOracleId             :: HKD f Text
               , _cardPrintsSearchUri      :: HKD f Text
               , _cardRulingsUri           :: HKD f Text
               , _cardScryfallUri          :: HKD f Text
               , _cardUri                  :: HKD f Text
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
               , _cardCardBackId           :: HKD f Text
               , _cardCollectorNumber      :: HKD f Text
               , _cardDigital              :: HKD f Bool
               , _cardFlavorText           :: HKD f Text
               , _cardFrameEffects         :: HKD f (Vector Text) --new data type?
               , _cardFrame                :: HKD f Text --new data type?
               , _cardFullArt              :: HKD f Bool
               , _cardGames                :: HKD f (Vector Game)
               , _cardHighresImage         :: HKD f Bool
               , _cardIllustrationId       :: HKD f Text
               , _cardImageUris            :: HKD f (HashMap Text Text)
               , _cardPrices               :: HKD f (HashMap Text Text)
               , _cardPrintedName          :: HKD f Text
               , _cardPrintedText          :: HKD f Text
               , _cardPrintedTypeLine      :: HKD f Text
               , _cardPromo                :: HKD f Bool
               , _cardPromoTypes           :: HKD f (Vector Text) --new data type?
               , _cardPurchaseUris         :: HKD f (HashMap Text Text)
               , _cardRarity               :: HKD f Text --new data type?
               , _cardRelatedUris          :: HKD f (HashMap Text Text)
               , _cardReleasedAt           :: HKD f Day
               , _cardReprint              :: HKD f Bool
               , _cardScryfallSetUri       :: HKD f Text
               , _cardSetName              :: HKD f Text
               , _cardSetSearchUri         :: HKD f Text
               , _cardSetType              :: HKD f Text
               , _cardSetUri               :: HKD f Text
               , _cardSet                  :: HKD f Text
               , _cardStorySpotlight       :: HKD f Bool
               , _cardTextless             :: HKD f Bool
               , _cardVariation            :: HKD f Bool
               , _cardVariationOf          :: HKD f Text
               , _cardWatermark            :: HKD f Text
               , _cardPreview              :: HKD f (HashMap Text Text) --new data type?
               } deriving (Generic)
