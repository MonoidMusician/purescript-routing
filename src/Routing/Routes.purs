module Routing.Routes where

import Prelude
import Routing.Match.Class (class MatchClass, end, int, lit)

import Control.Apply (applyFirst, applySecond)
import Control.Plus (empty, (<|>))
import Data.Array (fromFoldable)
import Data.Either (hush)
import Data.Foldable (foldMap)
import Data.Functor.Contravariant (class Contravariant)
import Data.Lens (_Just, _Nothing, preview, review)
import Data.Lens.Prism (prism', only)
import Data.Lens.Types (Prism')
import Data.Map (singleton, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Routing (match)
import Routing.Match (Match)
import Routing.Types (Route, RoutePart(..))

class Combinators c where
  emptyVal :: forall a. c a
  eitherOr :: forall a. c a -> c a -> c a
  prismMap :: forall a b. Prism' a b -> (c b -> c a)
  withCurry :: forall a b. c a -> c b -> c (Tuple a b)
  andThen :: forall b. c Unit -> c b -> c b
  before :: forall a. c a -> c Unit -> c a

infixl 3 eitherOr as <||>
infix 5 prismMap as <\>
infixr 6 withCurry as </>
infixl 7 andThen as />
infixl 7 before as </

instance combmatch :: Combinators Match where
  emptyVal = empty
  eitherOr = (<|>)
  prismMap p = map (review p)
  withCurry a b = Tuple <$> a <*> b
  andThen = applySecond
  before = applyFirst

newtype Routerify a = Routerify (a -> Route)
derive instance newtypeRouterify :: Newtype (Routerify a) _

instance contrafunctorRouterify :: Contravariant Routerify where
  cmap f (Routerify p) = Routerify (p <<< f)

instance combRouterify :: Combinators Routerify where
  emptyVal = Routerify (const mempty)
  eitherOr (Routerify l) (Routerify r) = Routerify \a -> l a <> r a
  prismMap p (Routerify r) = Routerify (foldMap r <<< preview p)
  withCurry (Routerify l) (Routerify r) = Routerify \(Tuple a b) ->
    l a <> r b
  andThen (Routerify l) (Routerify r) = Routerify \b ->
    l unit <> r b
  before (Routerify l) (Routerify r) = Routerify \a ->
    l a <> r unit

instance matchclassRouterify :: MatchClass Routerify where
  lit = Routerify <<< const <<< pure <<< Path
  num = Routerify (pure <<< Path <<< show)
  int = Routerify (pure <<< Path <<< show)
  bool = Routerify (pure <<< Path <<< if _ then "true" else "false")
  str = Routerify (pure <<< Path)
  end = Routerify (const empty)
  fail = Routerify <<< const <<< pure <<< Path
  param p = Routerify (pure <<< Query <<< singleton p)
  params = Routerify (pure <<< Query)

data Locations
  = Home
  | Dashboard
  | Project Int
derive instance eqLocations :: Eq Locations
instance showLocations :: Show Locations where
  show Home = "Home"
  show Dashboard = "Dashboard"
  show (Project i) = "Project #" <> show i
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

slash :: forall m. MatchClass m => m Unit
slash = lit ""
dir :: forall m. Combinators m => MatchClass m => m Unit
dir = slash /> end

loc :: forall m. Combinators m => MatchClass m => m Location
loc = _Home <\> dir
 <||> _Dashboard <\> slash /> lit "dashboard" /> end
 <||> _Project <\> slash /> lit "project" /> int </ end
 <||> _NotFound <\> slash /> lit "404" /> end

parseloc :: String -> Location
parseloc = match loc >>> hush >>> join

showloc :: Location -> String
showloc l =
  joinWith "/" $ fromFoldable $ unwrap (loc :: Routerify Location) l <#>
    case _ of
      Path p -> p
      Query q -> "?" <> joinWith "&"
        (toUnfoldable q <#> \(Tuple p v) -> p <> "=" <> v)
