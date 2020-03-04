{-# LANGUAGE RankNTypes, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}
module Data.Scryfall.Internal
  ( HKD
  , LensFor(..)
  , getLenses) where

import Data.Functor.Identity
import GHC.Generics

import Data.Aeson
import Data.Profunctor
import Data.Text (unpack)
    
-- borrowed from lens-4.18.1 (C) 2013-16 Edward Kmett

type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

iso :: (s -> a) -> (b -> t) -> (forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t))
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

-- Free lenses for Higher-Kinded Data as described by Sandy Maguire
-- https://reasonablypolymorphic.com/blog/free-lenses/

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data LensFor s a = LensFor { getLensFor :: Lens' s a }

class GLenses z i o where
  glenses :: Lens' (z Identity) (i p) -> o p

instance GLenses z (K1 _x a) (K1 _x (LensFor (z Identity) a)) where
  glenses l = K1 (LensFor (\f -> l $ fmap K1 . f . unK1))

instance (GLenses z i o) => GLenses z (M1 _a _b i) (M1 _a _b o) where
  glenses l = M1 (glenses (\f -> l $ fmap M1 . f. unM1))

instance (GLenses z i o, GLenses z i' o') => GLenses z ((:*:) i i') ((:*:) o o') where
  glenses l = glenses (\f -> l (\(a :*: b) -> fmap (:*: b) $ f a))
    :*: glenses (\f -> l (\(a :*: b) -> fmap (a :*:) $ f b))

instance GLenses z V1 V1 where
  glenses l = error "absent type found in lens generation"
  
instance GLenses z U1 U1 where
  glenses l = U1

getLenses
    :: forall z
     . ( Generic (z Identity)
       , Generic (z (LensFor (z Identity)))
       , GLenses z (Rep (z Identity))
                   (Rep (z (LensFor (z Identity))))
       )
    => z (LensFor (z Identity))
getLenses = to $ glenses @z $ iso from to
