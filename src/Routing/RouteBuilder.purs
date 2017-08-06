module Routing.RouteBuilder (RouteBuilder(..), build, format) where

import Control.Plus (empty, (<|>))
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Lens.SemiIso (apply)
import Data.List (List(..), (:))
import Data.Map (isEmpty, singleton, toUnfoldable)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Semiring.Free (Free, free)
import Data.String (drop)
import Data.Tuple (Tuple(..), uncurry)
import Data.Validation.Semiring (V, invalid, unV)
import Partial.Unsafe (unsafePartialBecause)
import Prelude hiding (discard, apply)
import Routing.Combinators (class Combinators, emptySuccess, withCurry)
import Routing.Match.Class (class MatchClass)
import Routing.Types (Route, RoutePart(..))

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
  semiMap p (RouteBuilder r) = RouteBuilder
    (apply p >>> either (invalid <<< free) r)
  withCurry (RouteBuilder l) (RouteBuilder r) = RouteBuilder
    (bimap l r >>> uncurry append)
  andThen l r = cmap (Tuple unit) (withCurry l r)
  before l r = cmap (Tuple <@> unit) (withCurry l r)
  list (RouteBuilder r) = RouteBuilder \as -> foldMap r as

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

-- | Show a `Route` as a `String`.
format :: Route -> String
format r = go r
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

-- | Build a value, returning a string or an error.
build :: forall a. RouteBuilder a -> a -> Either (Free String) String
build builder = unwrap builder >>> unV Left (Right <<< format)
