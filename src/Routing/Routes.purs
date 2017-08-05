module Routing.Routes where

import Routing.Combinators (class Combinators, allowed, discard, matchlit, (/>), (<:>), (<=/../>))

import Data.Array (fromFoldable)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Lens.Prism (prism', only)
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Types (Prism')
import Data.List (List(..), (:))
import Data.Map (isEmpty, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semiring.Free (Free(..))
import Data.String (drop, joinWith)
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (unV)
import Partial.Unsafe (unsafePartialBecause)
import Prelude hiding (discard)
import Routing (match)
import Routing.Match.Class (class MatchClass, int, lit, str)
import Routing.RouteBuilder (RouteBuilder)
import Routing.Types (Route, RoutePart(..))

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
