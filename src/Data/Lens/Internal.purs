module Data.Lens.Internal.Retail where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)

data Retail f s t a b = Retail (a -> f s) (t -> f b)

instance profunctorRetail :: Functor f => Profunctor (Retail f s t) where
  dimap f g (Retail l r) = Retail (l <<< f) (map g <<< r)

instance choiceRetailEither :: Choice (Retail (Either String) s t) where
  left (Retail as st) = Retail
    (either as (\_ -> Left "semi-iso failed")) (map Left <<< st)
  right (Retail as st) = Retail
    (either (\_ -> Left "semi-iso failed") as) (map Right <<< st)

instance choiceRetailMaybe :: Choice (Retail Maybe s t) where
  left (Retail as st) = Retail
    (either as (const Nothing)) (map Left <<< st)
  right (Retail as st) = Retail
    (either (const Nothing) as) (map Right <<< st)
