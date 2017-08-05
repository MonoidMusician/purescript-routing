module Routing.RouteBuilder (RouteBuilder(..)) where

import Control.Plus (empty, (<|>))
import Data.Bifunctor (bimap)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Lens (preview)
import Data.Map (singleton)
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Semiring.Free (Free, free)
import Data.Tuple (Tuple(..), uncurry)
import Data.Validation.Semiring (V, invalid)
import Prelude hiding (discard)
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
