module Data.Lens.SemiIso where

import Prelude

import Data.Either (Either)
import Data.Either (hush, note) as E
import Data.Identity (Identity)
import Data.Lens (APrism, AnIso, Iso, Optic, iso, withIso, withPrism)
import Data.Lens.Internal.Retail (Retail(..))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Profunctor.Exposed (class Exposed, expose, merge)

type SemiIso f s t a b = forall p. Exposed f p => Optic p s t a b
type SemiIso' f s a = SemiIso f s s a a

type ASemiIso f s t a b = Retail f a b a b -> Retail f a b s t
type ASemiIso' f s a = ASemiIso f s s a a

semiIso :: forall f s t a b. (s -> f a) -> (b -> f t) -> SemiIso f s t a b
semiIso sa bt = merge <<< dimap sa bt <<< expose

apply :: forall f s t a b. Applicative f => ASemiIso f s t a b -> s -> f a
apply l = withSemiIso l const

-- | Applies the 'SemiIso' in the opposite direction.
unapply :: forall f s t a b. Applicative f => ASemiIso f s t a b -> b -> f t
unapply l = withSemiIso l (const id)

withSemiIso :: forall f s t a b r. Applicative f => ASemiIso f s t a b -> ((s -> f a) -> (b -> f t) -> r) -> r
withSemiIso ai k = case ai (Retail pure pure) of
  Retail sa bt -> k sa bt

fromPrism :: forall s t a b. APrism s t a b -> SemiIso Maybe s t a b
fromPrism l = withPrism l \bt sa -> semiIso (E.hush <<< sa) (pure <<< bt)

note :: forall s t a b e. e -> ASemiIso Maybe s t a b -> SemiIso (Either e) s t a b
note e l = withSemiIso l \sa bt -> semiIso (E.note e <<< sa) (E.note e <<< bt)

hush :: forall s t a b e. ASemiIso (Either e) s t a b -> SemiIso Maybe s t a b
hush l = withSemiIso l \sa bt -> semiIso (E.hush <<< sa) (E.hush <<< bt)

weaken :: forall f s t a b. Applicative f => AnIso s t a b -> SemiIso f s t a b
weaken l = withIso l \sa bt -> semiIso (pure <<< sa) (pure <<< bt)

strengthen :: forall s t a b. ASemiIso Identity s t a b -> Iso s t a b
strengthen l = withSemiIso l \sa bt -> iso (unwrap <<< sa) (unwrap <<< bt)

constant :: forall f a. Applicative f => a -> SemiIso' f Unit a
constant a = semiIso (pure (pure a)) (pure (pure unit))

reconstant :: forall f a. Applicative f => a -> SemiIso' f a Unit
reconstant a = semiIso (pure (pure unit)) (pure (pure a))
