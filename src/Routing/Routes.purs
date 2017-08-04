module Routing.Routes where

import Prelude

import Control.Apply (applyFirst, applySecond, lift2)
import Control.Plus (empty, (<|>))
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, hush, note)
import Data.Foldable (fold, foldMap)
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

instance matchclassRouterify :: MatchClass Routerify where
  lit = Routerify <<< const <<< ppPath
  num = Routerify (ppPath <<< show)
  int = Routerify (ppPath <<< show)
  bool = Routerify (ppPath <<< if _ then "true" else "false")
  str = Routerify (ppPath)
  end = Routerify (const (pure empty))
  fail = Routerify <<< const <<< Left
  param p = Routerify (pure <<< pure <<< Query <<< singleton p)
  params = Routerify (pure <<< pure <<< Query)

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
slashish :: forall m a. Combinators m => MatchClass m => m a -> m a
slashish m = slash /> m <||> m
dir :: forall m. Combinators m => MatchClass m => m Unit
dir = slash /> end
dirish :: forall m. Combinators m => MatchClass m => m Unit
dirish = end <||> dir

loc :: forall m. Combinators m => MatchClass m => m Location
loc =  _Home      <\> slashish (end  <||> lit "home" /> dirish)
  <||> _Dashboard <\> slashish (lit "dashboard"      /> dirish)
  <||> _Project   <\> slashish (lit "project" /> int </ dirish)
  <||> _NotFound  <\> slashish (lit "404"            /> dirish)

parseloc :: String -> Location
parseloc = match loc >>> hush >>> join

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

showloc :: Location -> String
showloc = unwrap (loc :: Routerify Location) >>> either id showroute
