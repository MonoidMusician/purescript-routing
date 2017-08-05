module Routing.Routes where

import Control.Apply (applyFirst, applySecond, lift2)
import Control.Plus (empty, (<|>))
import Data.Array (fromFoldable)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Lens (preview, review)
import Data.Lens.Prism (prism', only)
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Types (Prism')
import Data.List (List(..), (:))
import Data.Map (isEmpty, singleton, toUnfoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Semiring.Free (Free(..), free)
import Data.String (drop, joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Data.Validation.Semiring (V, invalid, unV)
import Partial.Unsafe (unsafePartialBecause)
import Prelude hiding (discard)
import Routing (match)
import Routing.Match (Match(..))
import Routing.Match.Class (class MatchClass, end, int, lit, str)
import Routing.Types (Route, RoutePart(..))

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
  -- | `(<:>)` A sort of map that works either covariantly or contravariantly
  -- | with a sort of partial isomorphism (c must be like an error monad in the
  -- | latter case), so parsing is guaranteed to put a value into the `Prism'`
  -- | and routing may possibly match the prismic case.
  prismMap :: forall a b. Prism' a b -> (c b -> c a)
  -- | `(</>)` Alternate definition of an applicative that suits this use better.
  withCurry :: forall a b. c a -> c b -> c (Tuple a b)
  -- | `(/>)` C.f. Control.Apply.applySecond. Need a unit value to provide it
  -- | contravariantly.
  andThen :: forall b. c Unit -> c b -> c b
  -- | `(</)` C.f. Control.Apply.applyFirst.
  before :: forall a. c a -> c Unit -> c a

infixl 3 eitherOr as <||>
infix 5 prismMap as <:>
infixr 6 withCurry as </>
infixl 7 andThen as />
infixl 7 before as </

-- | Matching is an instance of Combinators, works like you would expect.
instance combmatch :: Combinators Match where
  emptyFail = empty
  emptySuccess = Match (pure <<< (Tuple <@> unit))
  allowed f = optional f
  eitherOr = (<|>)
  prismMap p = map (review p)
  withCurry = lift2 Tuple
  andThen = applySecond
  before = applyFirst

-- | A contravariant functor for building up a route to be converted to a string.
newtype RouteBuilder a = RouteBuilder (a -> V (Free String) Route)
derive instance newtypeRouteBuilder :: Newtype (RouteBuilder a) _

instance contrafunctorRouteBuilder :: Contravariant RouteBuilder where
  cmap f (RouteBuilder p) = RouteBuilder (p <<< f)

instance combRouteBuilder :: Combinators RouteBuilder where
  emptyFail = RouteBuilder (const (invalid (free "no match")))
  emptySuccess = RouteBuilder (const (pure mempty))
  allowed _ = emptySuccess
  eitherOr (RouteBuilder l) (RouteBuilder r) = RouteBuilder \a -> l a <|> r a
  prismMap p (RouteBuilder r) = RouteBuilder
    (preview p >>> maybe (invalid (free "prism failed to match")) r)
  withCurry (RouteBuilder l) (RouteBuilder r) = RouteBuilder
    (bimap l r >>> uncurry append)
  andThen l r = cmap (Tuple unit) (withCurry l r)
  before l r = cmap (Tuple <@> unit) (withCurry l r)

ppPath :: forall a e. Applicative a => Applicative e => String -> a (e RoutePart)
ppPath = pure <<< pure <<< Path

-- | RouteBuilder also can provide the same methods in `MatchClass`.
instance matchclassRouteBuilder :: MatchClass RouteBuilder where
  -- | Add a URL part.
  lit = RouteBuilder <<< const <<< ppPath
  -- | Show a `Number` as a URL part.
  num = RouteBuilder (ppPath <<< show)
  -- | Show an `Int` as a URL part.
  int = RouteBuilder (ppPath <<< show)
  -- | Show a `Boolean` as a URL part ("true" and "false").
  bool = RouteBuilder (ppPath <<< if _ then "true" else "false")
  -- | Show a `String` as a URL part.
  str = RouteBuilder (ppPath)
  -- | No-op, would have semantic value when matching.
  end = RouteBuilder (const (pure empty))
  -- | Provide an error message.
  fail = RouteBuilder <<< const <<< invalid <<< free
  -- | Show a single parameter.
  param p = RouteBuilder (pure <<< pure <<< Query <<< singleton p)
  -- | Show a bunch of parameters.
  params = RouteBuilder (pure <<< pure <<< Query)

-- | A sample ADT for a location.
data Locations
  = Home
  | Dashboard
  | Project Int
derive instance eqLocations :: Eq Locations
derive instance ordLocations :: Ord Locations
instance showLocations :: Show Locations where
  show Home = "Home"
  show Dashboard = "Dashboard"
  show (Project i) = "Project " <> show i
-- | A `Left` value indicates a 404 not found error. See `Prism'`s below.
type Location = Either String Locations

_Home :: Prism' Location Unit
_Home = _Right <<< only Home
_Dashboard :: Prism' Location Unit
_Dashboard = _Right <<< only Dashboard
_Project :: Prism' Location Int
_Project = _Right <<< prism' Project case _ of
  Project i -> Just i
  _ -> Nothing
_NotFound :: Prism' Location String
_NotFound = _Left

-- | A slash separating parts of a path.
slash :: forall m. MatchClass m => m Unit
slash = lit ""
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
route :: forall b a m. Combinators m => MatchClass m => Prism' a b -> m b -> m a
route p r = p <:> slash' (r </ dir')
infix 4 route as <=/../>
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

-- | The `MatchClass` value for a `Location`.
-- |   - `_Home` is "/" or "/home", etc.
-- |   - `_Dashboard` is "dashboard", potentially starting or ending with a slash.
-- |   - `_Project` is "project/{int}", idem
-- |   - `_NotFound` maps to `404` (idem), but can represent any other location.
loc :: forall m. Combinators m => MatchClass m => m Location
loc = do
  _Home      <=/../> allowed (lit "home")
  _Dashboard <=/../> lit "dashboard"
  _Project   <=/../> lit "project" /> int
  _NotFound  <=/../> match404
  where
    match404 = do
      -- "404" --> 404
      matchlit "404"
      -- "" --> 404
      only "" <:> lit "404"
      -- reason --> 404/{reason}
      lit "404" /> str

-- | Parses a `String` into a `Location`, giving any failures to `_NotFound`
-- | aka `Left`.
parseloc :: String -> Location
parseloc l = match loc l # either (const (Left ("url_parse_error/" <> l))) id

-- | Show a `Route` as a `String`.
showroute :: Route -> String
showroute r = go r
  where
    asList = id :: List ~> List
    go = case _ of
      Nil -> ""
      Path p : Nil -> p
      Path p : tail ->
        p <> "/" <> go tail
      whole@(Query q : tail)
        | isEmpty q -> go tail
        | otherwise ->
          "?" <> drop 1 (goquery whole)
    goquery =
      unsafePartialBecause
        "query strings should not contain path elements"
        case _ of
          Nil -> ""
          Query q : tail ->
            showquery q <> goquery tail
    showquery = toUnfoldable >>> asList >>> foldMap
      \(Tuple p v) -> "&" <> p <> "=" <> v

-- | Show a `Location` as a `String`. Should be the partial inverse of
-- | `parseloc`. `_NotFound` gets rendered as "/404/...".
-- | Errors are, somewhat unforunately, printed into the result string ...
showloc :: Location -> String
showloc = unwrap (loc :: RouteBuilder Location) >>> flip unV showroute \(Free e) ->
  showerrorzies e

showerrorzies :: List (List String) -> String
showerrorzies e = joinWith "; " $ fromFoldable $ joinWith ", " <<< fromFoldable <$> e
