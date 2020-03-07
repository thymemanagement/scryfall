{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Data.Scryfall
  ( Color(..)
  , Legality(..)
  , Game(..)
  , CardFace
  , CardFaceF(..)
  , cardFaceLenses
  , RelatedCard
  , RelatedCardF(..)
  , relatedCardLenses
  , Card
  , CardF(..)
  , cardLenses
  , Catalog
  , CatalogF(..)
  , catalogLenses
  , Error
  , ErrorF(..)
  , errorLenses
  , List
  , ListF(..)
  , Ruling
  , RulingF(..)
  , rulingLenses
  , Symbol
  , SymbolF(..)
  , symbolLenses
  , Mana
  , ManaF(..)
  , manaLenses
  , Set
  , SetF(..)
  , setLenses
  , LensFor(..)) where

import Data.Scryfall.Card
import Data.Scryfall.Internal
import Data.Scryfall.JSON
import Data.Scryfall.Misc
import Data.Scryfall.Set
import Data.Scryfall.Symbol

import Data.Functor.Identity
import GHC.Generics

import Data.Aeson
import Data.Text (unpack)

--color

instance FromJSON Color where
  parseJSON = withText "Color" $ \ case
    "W" -> pure White
    "U" -> pure Blue
    "B" -> pure Black
    "R" -> pure Red
    "G" -> pure Green
    str -> fail (unpack str ++ "does not represent a color")

--card and card properties

instance FromJSON Legality where
  parseJSON = withText "Legality" $ \ case
    "legal" -> pure Legal
    "not_legal" -> pure NotLegal
    "restricted" -> pure Restricted
    "banned" -> pure Banned
    str -> fail (unpack str ++ "does not represent a legality")

instance FromJSON Game where
  parseJSON = withText "Color" $ \ case
    "paper" -> pure Paper
    "mtgo" -> pure MTGO
    "arena" -> pure Arena
    str -> fail (unpack str ++ "does not represent a game")

type CardFace = CardFaceF Identity

deriving instance Eq (CardFaceF Identity)
deriving instance Ord (CardFaceF Identity)
deriving instance Read (CardFaceF Identity)
deriving instance Show (CardFaceF Identity)

sfGenerateFromJSON ''CardFaceF

cardFaceLenses :: CardFaceF (LensFor CardFace)
cardFaceLenses = getLenses

type RelatedCard = RelatedCardF Identity

deriving instance Eq (RelatedCardF Identity)
deriving instance Ord (RelatedCardF Identity)
deriving instance Read (RelatedCardF Identity)
deriving instance Show (RelatedCardF Identity)

sfGenerateFromJSON ''RelatedCardF

relatedCardLenses :: RelatedCardF (LensFor RelatedCard)
relatedCardLenses = getLenses

type Card = CardF Identity

deriving instance Eq (CardF Identity)
deriving instance Ord (CardF Identity)
deriving instance Read (CardF Identity)
deriving instance Show (CardF Identity)

sfGenerateFromJSON ''CardF

cardLenses :: CardF (LensFor Card)
cardLenses = getLenses

--catalog

type Catalog = CatalogF Identity

deriving instance Eq (CatalogF Identity)
deriving instance Ord (CatalogF Identity)
deriving instance Read (CatalogF Identity)
deriving instance Show (CatalogF Identity)

sfGenerateFromJSON ''CatalogF

catalogLenses :: CatalogF (LensFor Catalog)
catalogLenses = getLenses

--error

type Error = ErrorF Identity

deriving instance Eq (ErrorF Identity)
deriving instance Ord (ErrorF Identity)
deriving instance Read (ErrorF Identity)
deriving instance Show (ErrorF Identity)

sfGenerateFromJSON ''ErrorF

errorLenses :: ErrorF (LensFor Error)
errorLenses = getLenses

--list

type List a = ListF Identity a

deriving instance Eq a => Eq (ListF Identity a)
deriving instance Ord a => Ord (ListF Identity a)
deriving instance Read a => Read (ListF Identity a)
deriving instance Show a => Show (ListF Identity a)

sfGenerateFromJSON ''ListF

-- TODO
{- listLenses :: ListF (LensFor (List a)) a
listLenses = getLenses -}

--ruling

type Ruling = RulingF Identity

deriving instance Eq (RulingF Identity)
deriving instance Ord (RulingF Identity)
deriving instance Read (RulingF Identity)
deriving instance Show (RulingF Identity)

sfGenerateFromJSON ''RulingF

rulingLenses :: RulingF (LensFor Ruling)
rulingLenses = getLenses

--symbol

type Symbol = SymbolF Identity

deriving instance Eq (SymbolF Identity)
deriving instance Ord (SymbolF Identity)
deriving instance Read (SymbolF Identity)
deriving instance Show (SymbolF Identity)

sfGenerateFromJSON ''SymbolF

symbolLenses :: SymbolF (LensFor Symbol)
symbolLenses = getLenses

--mana

type Mana = ManaF Identity

deriving instance Eq (ManaF Identity)
deriving instance Ord (ManaF Identity)
deriving instance Read (ManaF Identity)
deriving instance Show (ManaF Identity)

sfGenerateFromJSON ''ManaF

manaLenses :: ManaF (LensFor Mana)
manaLenses = getLenses

--set

type Set = SetF Identity

deriving instance Eq (SetF Identity)
deriving instance Ord (SetF Identity)
deriving instance Read (SetF Identity)
deriving instance Show (SetF Identity)

sfGenerateFromJSON ''SetF

setLenses :: SetF (LensFor Set)
setLenses = getLenses


