module Routing.Routes where

import Data.Either (Either(..), either)
import Data.Lens.Prism (prism', only)
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Types (Prism')
import Data.Maybe (Maybe(..))
import Prelude hiding (discard)
import Routing (match)
import Routing.Combinators (class Combinators, allowed, discard, matchlit, (/>), (<:>), (<:/.../>))
import Routing.Match.Class (class MatchClass, int, lit, str)
import Routing.RouteBuilder (build)

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
  _Home      <:/.../> allowed (lit "home")
  _Dashboard <:/.../> lit "dashboard"
  _Project   <:/.../> lit "project" /> int
  _NotFound  <:/.../> match404
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

-- | Show a `Location` as a `String`. Should be the partial inverse of
-- | `parseloc`. `_NotFound` gets rendered as "/404/...".
-- | Errors are, somewhat unforunately, printed into the result string ...
showloc :: Location -> String
showloc = build loc >>> either id id
