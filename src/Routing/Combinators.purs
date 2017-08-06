module Routing.Combinators where

import Data.Either (Either)
import Data.Functor ((<@>))
import Data.Lens.Iso (iso, re)
import Data.Lens.Prism (APrism', only)
import Data.Lens.SemiIso (SemiIso', fromPrism, note)
import Data.List (List)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (Tuple2, tuple2, uncurry2)
import Data.Unit (Unit, unit)
import Routing.Match.Class (class MatchClass, end, lit, param, slash)

-- | Mostly duplicated operations from elsewhere bundled up to work similar to
-- | http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf
class Combinators c where
  -- | C.f. Control.Alt.empty.
  emptyFail :: forall a. c a
  -- | Uh don't ask me...
  emptySuccess :: c Unit
  -- | The url may include this but it will not be generated.
  -- | This has different semantics for matching versus displaying ...
  allowed :: c Unit -> c Unit
  -- | `(<||>)` C.f. Control.Alt.alt.
  eitherOr :: forall a. c a -> c a -> c a
  -- | `(<=>)` A sort of map that works either covariantly or contravariantly
  -- | with a partial isomorphism.
  semiMap :: forall a b. SemiIso' (Either String) a b -> (c a -> c b)
  -- | `(</>)` Alternate definition of an applicative that suits this use better.
  withCurry :: forall a b. c a -> c b -> c (Tuple a b)
  -- | `(/>)` C.f. Control.Apply.applySecond. Need a unit value to provide it
  -- | contravariantly.
  andThen :: forall b. c Unit -> c b -> c b
  -- | `(</)` C.f. Control.Apply.applyFirst.
  before :: forall a. c a -> c Unit -> c a
  list :: forall a. c a -> c (List a)

infixl 3 eitherOr as <||>
infix 5 semiMap as <=>
infix 5 prismMap as <:>
infixr 6 withCurry as </>
infixl 7 andThen as />
infixl 7 before as </

-- | Parsing is guaranteed to put a value into the `Prism'`
-- | and routing may possibly match the prismic case.
prismMap :: forall c a b. Combinators c => APrism' a b -> (c b -> c a)
prismMap l = semiMap (re (note "prism failed" (fromPrism l)))

-- | End a nested tuple.
endCurry :: forall c a b. Combinators c => c a -> c b -> c (Tuple2 a b)
endCurry ca cb = iso (uncurry tuple2) (uncurry2 Tuple) <=> ca </> cb
infixr 6 endCurry as <&>

-- | Prefer a slash prefixing the combinator.
slash' :: forall m a. Combinators m => MatchClass m => m a -> m a
slash' m = slash /> m <||> m

-- | A directory ends with a slash.
dir :: forall m. Combinators m => MatchClass m => m Unit
dir = slash /> end

-- | This may end with a slash.
dir' :: forall m. Combinators m => MatchClass m => m Unit
dir' = end <||> dir

-- | Surrounds a prismic route with `slash'` and `dir'`.
route :: forall b a m. Combinators m => MatchClass m => APrism' a b -> m b -> m a
route p r = p <:> slash' (r </ dir')
infix 4 route as <:/.../>

-- | The url should include this (i.e. it will be printed), but it does not have
-- | to.
optional :: forall m. Combinators m => MatchClass m => m Unit -> m Unit
optional = eitherOr <@> emptySuccess

-- | Match a literal and return it as a `String`.
matchlit :: forall m. Combinators m => MatchClass m => String -> m String
matchlit v = only v <:> lit v

-- | Join a list of combinators with `eitherOr`.
discard :: forall m a. Combinators m => m a -> (Unit -> m a) -> m a
discard a f = a <||> f unit

-- | Use a `Prism'` to parse a parameter into a value or show it.
paramWith :: forall m a. Combinators m => MatchClass m => APrism' String a -> String -> m a
paramWith p k = siso <=> param k
  where siso = note "parsing parameter failed" (fromPrism p)

paramOf :: forall m a. Combinators m => MatchClass m => String -> APrism' String a -> m a
paramOf k p = paramWith p k
infix 8 paramOf as ?::
infix 8 paramOf as &::
