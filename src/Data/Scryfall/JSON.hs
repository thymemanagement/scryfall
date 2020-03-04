{-# LANGUAGE OverloadedStrings, TemplateHaskell, LambdaCase #-}
module Data.Scryfall.JSON (sfGenerateFromJSON) where

import Control.Applicative
import Data.Char
import Data.Functor.Identity
import Data.List
import Data.Monoid

import Data.Aeson
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

getJSONField :: Name -> Name -> Q String
getJSONField cnName fdName = case stripPrefix ("_" ++ (headMap toLower . nameBase $ cnName)) (nameBase fdName) of
  Just str -> return $ camelTo2 '_' str
  Nothing -> fail $ (nameBase fdName) ++ "'s name is formatted incorrectly (constuctor name: " ++ (nameBase cnName) ++ ")"

liftText :: T.Text -> ExpQ
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

headMap :: (a -> a) -> [a] -> [a]
headMap _ [] = []
headMap f (x:xs) = (f x : xs)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

parseFieldSimple :: Name -> T.Text -> ExpQ
parseFieldSimple vName field = [e| $(varE vName) .: $(liftText field) |]

parseFieldAlternative :: Name -> T.Text -> ExpQ
parseFieldAlternative vName field = [e| $(varE vName) .:? $(liftText field) .!= empty |]

parseFieldMonoid :: Name -> T.Text -> ExpQ
parseFieldMonoid vName field = [e| $(varE vName) .:? $(liftText field) .!= mempty |]

parseFieldJSON :: Name -> Name -> Name -> ExpQ
parseFieldJSON vName cnName fdName = do
  fieldType <- reify fdName >>= \ case
    VarI _ (ForallT _ _ (AppT _ (AppT _ t))) _ -> return t
    _ -> fail $ (nameBase fdName) ++ "representation not formatted correctly to parse JSON"
  fieldName <- T.pack <$> getJSONField cnName fdName
  isAlternative <- case fieldType of
    AppT t1 t2 -> isInstance ''Alternative [t1]
    _ -> return False
  isMonoid <- isInstance ''Monoid [fieldType]
  if isAlternative
    then parseFieldAlternative vName fieldName
    else if isMonoid
         then parseFieldMonoid vName fieldName
         else parseFieldSimple vName fieldName

sfParseJSON :: Name -> Name -> [Name] -> ExpQ
sfParseJSON tyName cnName fdNames = do
  vName <- newName "v"
  [e| withObject $(stringE . nameBase $ tyName) $ \ $(varP vName) ->
      $(foldl1 (\acc x -> [e| $acc <*> $x |])
        (headMap (\h -> [e| $(conE cnName) <$> $h |]) ((parseFieldJSON vName cnName) <$> fdNames))) |]

sfFromJSONContext :: [Name] -> CxtQ
sfFromJSONContext = return . fmap (AppT (ConT ''FromJSON) . VarT)

sfInstantiateType :: Name -> [Name] -> TypeQ
sfInstantiateType tyName = return . AppT (ConT ''FromJSON) . foldl' (\acc x -> AppT acc (VarT x)) (AppT (ConT tyName) (ConT ''Identity))

sfGenerateFromJSON :: Name -> DecsQ
sfGenerateFromJSON tyName = do
  (boundTVs, cnName, fdNames) <- reify tyName >>= \ case
    TyConI (DataD _ _ tvBinders _ [RecC recName fdBangTypes] _) ->
      return (length (drop 1 tvBinders), recName, fst3 <$> fdBangTypes)
    _ -> fail $ (nameBase tyName) ++ "cannot be parsed as JSON"
  newTVars <- sequence $ replicate boundTVs (newName "a")
  pure <$> instanceD (sfFromJSONContext newTVars) (sfInstantiateType tyName newTVars)
    [valD (varP 'parseJSON) (normalB (sfParseJSON tyName cnName fdNames)) []]
