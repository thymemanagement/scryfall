{-# LANGUAGE TypeFamilies, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Data.Scryfall.Set (SetF(..)) where

import Data.Scryfall.Internal

import GHC.Generics

import Data.Text
import Data.Time


data SetF f = Set
              { _setId            :: HKD f Text
              , _setCode          :: HKD f Text
              , _setMtgoCode      :: HKD f Text
              , _setTcgplayerId   :: HKD f (Maybe Integer)
              , _setName          :: HKD f Text
              , _setSetType       :: HKD f Text --new data type?
              , _setReleasedAt    :: HKD f (Maybe Day)
              , _setBlockCode     :: HKD f Text
              , _setBlock         :: HKD f Text
              , _setParentSetCode :: HKD f Text
              , _setCardCount     :: HKD f Integer
              , _setDigital       :: HKD f Bool
              , _setScryfallUri   :: HKD f Text
              , _setUri           :: HKD f Text
              , _setIconSvgUri    :: HKD f Text
              , _setSearchUri     :: HKD f Text
              } deriving (Generic)
