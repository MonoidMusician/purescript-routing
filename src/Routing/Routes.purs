module Routing.Routes where

import Prelude

import Control.Apply (applyFirst, applySecond, lift2)
import Control.Plus (empty, (<|>))
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, hush, note)
import Data.Foldable (foldMap)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Lens (_Just, _Nothing, preview, review)
import Data.Lens.Prism (prism', only)
import Data.Lens.Types (Prism')
import Data.List (List(..), (:))
import Data.Map (isEmpty, singleton, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (drop)
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartialBecause)
import Routing (match)
import Routing.Match (Match)
import Routing.Match.Class (class MatchClass, end, int, lit)
import Routing.Types (Route, RoutePart(..))

-- | Mostly duplicated operations from elsewhere bundled up to work similar to
-- | http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf
class Combinators c where
  -- | C.f. Control.Alt.empty.
  emptyVal :: forall a. c a
  -- | (<||>) C.f. Control.Alt.alt.
  eitherOr :: forall a. c a -> c a -> c a
  -- | (<:>) A sort of map that works either covariantly or contravariantly with
  -- | a sort of partial isomorphism (c must be like an error monad in the
  -- | latter case), so parsing is guaranteed to put a value into the `Prism'`
  -- | and routing may possibly match the prismic case.
  prismMap :: forall a b. Prism' a b -> (c b -> c a)
  -- | (</>) Alternate applicative definition that suits this application better.
  withCurry :: forall a b. c a -> c b -> c (Tuple a b)
  -- | (/>) C.f. Control.Apply.applySecond. Need a unit value to provide it
  -- | contravariantly.
  andThen :: forall b. c Unit -> c b -> c b
  -- | (</) C.f. Control.Apply.applyFirst.
  before :: forall a. c a -> c Unit -> c a

infixl 3 eitherOr as <||>
infix 5 prismMap as <:>
infixr 6 withCurry as </>
infixl 7 andThen as />
infixl 7 before as </

-- | Matching is an instance of Combinators, works like you would expect.
instance combmatch :: Combinators Match where
  emptyVal = empty
  eitherOr = (<|>)
  prismMap p = map (review p)
  withCurry a b = Tuple <$> a <*> b
  andThen = applySecond
  before = applyFirst

-- | A contravariant functor for building up a route to be converted to a string.
newtype Routerify a = Routerify (a -> Either String Route)
derive instance newtypeRouterify :: Newtype (Routerify a) _

instance contrafunctorRouterify :: Contravariant Routerify where
  cmap f (Routerify p) = Routerify (p <<< f)

instance combRouterify :: Combinators Routerify where
  emptyVal = Routerify (const (Left "no match"))
  eitherOr (Routerify l) (Routerify r) = Routerify \a ->
    case l a, r a of
      Right la, _ -> Right la
      _, Right ra -> Right ra
      Left la, Left ra -> Left (la <> ra)
  prismMap p (Routerify r) = Routerify
    (preview p >>> map r >>> note "prism failed to match" >>> join)
  withCurry (Routerify l) (Routerify r) = Routerify
    (bimap l r >>> uncurry (lift2 append))
  andThen l r = cmap (Tuple unit) (withCurry l r)
  before l r = cmap (Tuple <@> unit) (withCurry l r)

ppPath :: forall a e. Applicative a => Applicative e => String -> a (e RoutePart)
ppPath = pure <<< pure <<< Path

-- | Routerify also can provide the same methods in `MatchClass`.
instance matchclassRouterify :: MatchClass Routerify where
  -- | Add a URL part.
  lit = Routerify <<< const <<< ppPath
  -- | Show a `Number` as a URL part.
  num = Routerify (ppPath <<< show)
  -- | Show an `Int` as a URL part.
  int = Routerify (ppPath <<< show)
  -- | Show a `Boolean` as a URL part.
  bool = Routerify (ppPath <<< if _ then "true" else "false")
  -- | Show a `String` as a URL part.
  str = Routerify (ppPath)
  -- | No-op, has semantic value when matching.
  end = Routerify (const (pure empty))
  -- | Provide an error message.
  fail = Routerify <<< const <<< Left
  -- | Show a single parameter.
  param p = Routerify (pure <<< pure <<< Query <<< singleton p)
  -- | Show a bunch of parameters.
  params = Routerify (pure <<< pure <<< Query)

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
-- | A `Nothing` value indicates a 404 not found error. See `Prism'`s below.
type Location = Maybe Locations

_Home :: Prism' Location Unit
_Home = _Just <<< only Home
_Dashboard :: Prism' Location Unit
_Dashboard = _Just <<< only Dashboard
_Project :: Prism' Location Int
_Project = _Just <<< prism' Project case _ of
  Project i -> Just i
  _ -> Nothing
_NotFound :: Prism' Location Unit
_NotFound = _Nothing

-- | A slash value.
slash :: forall m. MatchClass m => m Unit
slash = lit ""
-- | Prefer a slash prefixing the combinator.
slashish :: forall m a. Combinators m => MatchClass m => m a -> m a
slashish m = slash /> m <||> m
-- | A directory ends with a slash.
dir :: forall m. Combinators m => MatchClass m => m Unit
dir = slash /> end
-- | This may end with a slash.
dirish :: forall m. Combinators m => MatchClass m => m Unit
dirish = end <||> dir
-- | The url may include this but it will not be generated.
allowed :: forall m. Combinators m => MatchClass m => m Unit -> m Unit
allowed = eitherOr emptyVal
-- | The url should include this (i.e. it will be printed), but it does not have
-- | to.
optional :: forall m. Combinators m => MatchClass m => m Unit -> m Unit
optional = eitherOr <@> emptyVal

-- | The `MatchClass` value for a `Location`.
-- |   - `_Home` is "/" or "/home", etc.
-- |   - `_Dashboard` is "dashboard", potentially starting or ending with a slash.
-- |   - `_Project` is "project/{int}", idem
-- |   - `_NotFound` maps to `404` (idem), but can represent any other location.
loc :: forall m. Combinators m => MatchClass m => m Location
loc =  _Home      <:> slashish (allowed  (lit "home" /> dirish))
  <||> _Dashboard <:> slashish (lit "dashboard"      /> dirish)
  <||> _Project   <:> slashish (lit "project" /> int </ dirish)
  <||> _NotFound  <:> slashish (lit "404"            /> dirish)

-- | Parses a `String` into a `Location`, giving any failures to `_NotFound`
-- | aka `Nothing`.
parseloc :: String -> Location
parseloc = match loc >>> hush >>> join

-- | Show a `Route` as a `String`.
showroute :: Route -> String
showroute r = go r
  where
    asList = id :: List ~> List
    go = case _ of
      Nil -> ""
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
-- | `parseloc`. `_NotFound` gets rendered as "/404/".
showloc :: Location -> String
showloc = unwrap (loc :: Routerify Location) >>> either id showroute
