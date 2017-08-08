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

type ASemiIso f s t a b = Optic (Retail f a b) s t a b
type ASemiIso' f s a = ASemiIso f s s a a

semiIso :: forall f s t a b. (s -> f a) -> (b -> f t) -> SemiIso f s t a b
semiIso sfa bft = merge <<< dimap sfa bft <<< expose

apply :: forall f s t a b. Applicative f => ASemiIso f s t a b -> s -> f a
apply l = withSemiIso l const

-- | Applies the 'SemiIso' in the opposite direction.
unapply :: forall f s t a b. Applicative f => ASemiIso f s t a b -> b -> f t
unapply l = withSemiIso l (const id)

-- | Compose two `SemiIso'`s. In order to get this to respect typing it might be
-- | necessary to inline the definition of this, be warned!
compose :: forall f s a x. Bind f => ASemiIso' f x a -> ASemiIso' f s x -> SemiIso' f s a
compose xaiax sxixs =
  withSemiIso xaiax \xfa afx ->
    withSemiIso sxixs \sfx xfs ->
      semiIso
        -- once we have the transformations
        -- compose them Kleisli style
        (sfx >=> xfa) -- s -> f a
        (afx >=> xfs) -- a -> f s

-- | Unwraps the two functions contained in a `Retail`.
withSemiIso :: forall f s t a b r. Applicative f => ASemiIso f s t a b -> ((s -> f a) -> (b -> f t) -> r) -> r
withSemiIso stiab k =
  -- apply the steps of semiIso to Retail pure pure to get Retail f g
  --            Retail pure pure
  -- expose:    Retail (bind pure) pure
  -- a.k.a.:    Retail id pure
  -- dimap f g: Retail f (map g <<< pure)
  -- a.k.a.:    Retail f (pure <<< g)
  -- merge:     Retail f (join <<< pure <<< g)
  -- a.k.a.:    Retail f g
  case stiab (Retail pure pure) of
    Retail sfa bft -> k sfa bft

-- | Hoist the error monad that the semi-isomorphism operates in.
hoist :: forall f g s t a b. Applicative f => (f ~> g) -> ASemiIso f s t a b -> SemiIso g s t a b
hoist nt l = withSemiIso l \sfa bft -> semiIso (nt <<< sfa) (nt <<< bft)

note :: forall s t a b e. e -> ASemiIso Maybe s t a b -> SemiIso (Either e) s t a b
note e l = hoist (E.note e) l

hush :: forall s t a b e. ASemiIso (Either e) s t a b -> SemiIso Maybe s t a b
hush l = hoist E.hush l

-- | Every `Prism` is a `SemiIso`.
fromPrism :: forall s t a b. APrism s t a b -> SemiIso Maybe s t a b
fromPrism l = withPrism l \bt sa -> semiIso (E.hush <<< sa) (pure <<< bt)

-- | Every `Iso` is a `SemiIso`.
weaken :: forall f s t a b. Applicative f => AnIso s t a b -> SemiIso f s t a b
weaken l = withIso l \sa bt -> semiIso (pure <<< sa) (pure <<< bt)

-- | If the error monad is just `Identity`, then a `SemiIso` is really an `Iso`.
strengthen :: forall s t a b. ASemiIso Identity s t a b -> Iso s t a b
strengthen l = withSemiIso l \sfa bft -> iso (unwrap <<< sfa) (unwrap <<< bft)

-- | Always generates a `unit` value or the `a` passed in, depending on
-- | direction.
constant :: forall f a. Applicative f => a -> SemiIso' f Unit a
constant a = semiIso (const (pure a)) (const (pure unit))
