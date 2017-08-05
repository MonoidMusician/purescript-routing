module Test.Main where

import Prelude (class Show, class Eq, Unit, discard, show, ($), (<$>), (*>), (<*), (<*>), (<>), (==), append)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List)
import Data.Lens.Prism (Prism', prism')
import Data.Tuple (Tuple(..), uncurry)
import Data.Maybe (Maybe(..))
import Data.Map as M


import Routing (match)
import Routing.Match (Match)
import Routing.Match.Class (class MatchClass, bool, end, int, lit, num, param, params, slash)
import Routing.RouteBuilder (build)
import Routing.Combinators (class Combinators, (<=/../>), (</>), (/>), (</), list)
import Routing.Combinators (discard) as C

import Test.Assert (ASSERT, assert')

data FooBar
  = Foo Number (M.Map String String)
  | Bar Boolean String
  | Baz (List Number)
  | Quux Int
  | End Int
derive instance eqFooBar :: Eq FooBar

instance showFooBar :: Show FooBar where
  show (Foo num q) = "(Foo " <> show num <> " " <> show q <> ")"
  show (Bar bool str) = "(Bar " <> show bool <> " " <> show str <> ")"
  show (Baz lst) = "(Baz " <> show lst <> ")"
  show (Quux i) = "(Quux " <> show i <> ")"
  show (End i) = "(End " <> show i <> ")"

_Foo :: Prism' FooBar (Tuple Number (M.Map String String))
_Foo = prism' (uncurry Foo) case _ of
  Foo n m -> Just (Tuple n m)
  _ -> Nothing

_Bar :: Prism' FooBar (Tuple Boolean String)
_Bar = prism' (uncurry Bar) case _ of
  Bar b s -> Just (Tuple b s)
  _ -> Nothing

_Baz :: Prism' FooBar (List Number)
_Baz = prism' Baz case _ of
  Baz ns -> Just ns
  _ -> Nothing

_Quux :: Prism' FooBar Int
_Quux = prism' Quux case _ of
  Quux i -> Just i
  _ -> Nothing

_End :: Prism' FooBar Int
_End = prism' End case _ of
  End i -> Just i
  _ -> Nothing

routing :: Match FooBar
routing =
  Foo <$> (lit "foo" *> num) <*> params
    <|> Bar <$> (lit "bar" *> bool) <*> (param "baz")
    <|> Quux <$> (lit "" *> lit "quux" *> int)
    -- Order matters here.  `list` is greedy, and `end` wont match after it
    <|> End <$> (lit "" *> int <* end)
    <|> Baz <$> (list num)

bidi :: forall m. Combinators m => MatchClass m => m FooBar
bidi = let discard = C.discard in do
  _Foo <=/../> lit "foo" /> num </> params
  _Bar <=/../> lit "bar" /> bool </> param "baz"
  _Quux <=/../> slash /> lit "quux" /> int
  _End <=/../> slash /> int </ end
  _Baz <=/../> list num

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  print "Foo: " $ match bidi "foo/12/?welp='hi'&b=false" -- foo
  print "Quux: " $ match bidi "/quux/42" -- quux
  print "Baz: " $ match bidi "/123/" -- baz
  print "End: " $ match bidi "/1" -- end

  assert' "Roundtrip End: " $ (match bidi <$> (build bidi $ End 2)) == Right (Right $ End 2)

  where print s e = log $ append s $ show e

  -- (minimal test for browser)

  -- matches routing $ \old new -> void do
  --   logShow old
  --   logShow new
