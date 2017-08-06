module Data.Profunctor.Exposed where

import Control.Monad (class Monad)
import Control.Semigroupoid (compose, composeFlipped, (<<<))
import Data.Profunctor (class Profunctor)

class (Monad m, Profunctor p) <= Exposed m p | p -> m where
    expose :: forall a b. p a b -> p (m a) b
    merge  :: forall a b. p a (m b) -> p a b

exposed :: forall m p s t a b. Exposed m p => (p (m s) t -> p a (m b)) -> p s t -> p a b
exposed = compose merge <<< composeFlipped expose
