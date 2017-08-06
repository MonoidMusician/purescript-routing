module Data.Lens.SemiIso where

import Prelude

import Data.Lens (Optic)
import Data.Profunctor (dimap)
import Data.Profunctor.Exposed (class Exposed, expose, merge)

type SemiIso m s t a b = forall p. Exposed m p => Optic p s t a b

semiIso :: forall m s t a b. (s -> m a) -> (b -> m t) -> SemiIso m s t a b
semiIso sa bt = merge <<< dimap sa bt <<< expose
