{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Data.Scryfall.Symbol (Color(..), SymbolF(..), ManaF(..)) where

import Data.Scryfall.Internal

import GHC.Generics

import Data.Text
import Data.Vector

data Color = White | Blue | Black | Red | Green deriving (Generic,Eq,Ord,Read,Show)

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

data ManaF f = Mana
               { _manaCost                   :: HKD f Text
               , _manaCmc                    :: HKD f Double
               , _manaColors                 :: HKD f (Vector Color)
               , _manaColorless              :: HKD f Bool
               , _manaMonocolored            :: HKD f Bool
               , _manaMulticolored           :: HKD f Bool
               } deriving (Generic)
