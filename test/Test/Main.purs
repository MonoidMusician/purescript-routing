module Test.Main where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens.Prism (Prism', prism')
import Data.List (List(..), (:))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (Tuple3, tuple3, uncurry3)
import Prelude (class Eq, class Show, Unit, append, discard, show, ($), (*>), (<$>), (<*), (<*>), (<>), (==))
import Routing (match)
import Routing.Combinators (class Combinators, dir', list, (/>), (<&>), (</), (</>), (<:>))
import Routing.Combinators (discard) as C
import Routing.Match (Match)
import Routing.Match.Class (class MatchClass, bool, end, int, lit, num, param, params, slash)
import Routing.RouteBuilder (build)
import Test.Assert (ASSERT, assert)

data FooBar
  = Foo Number (M.Map String String)
  | Bar Boolean String String
  | Baz (List Number)
  | Quux Int
  | End Int
derive instance eqFooBar :: Eq FooBar

instance showFooBar :: Show FooBar where
  show (Foo num q) = "(Foo " <> show num <> " " <> show q <> ")"
  show (Bar bool str s) = "(Bar " <> show bool <> " " <> show str <> " " <> show s <> ")"
  show (Baz lst) = "(Baz " <> show lst <> ")"
  show (Quux i) = "(Quux " <> show i <> ")"
  show (End i) = "(End " <> show i <> ")"

_Foo :: Prism' FooBar (Tuple Number (M.Map String String))
_Foo = prism' (uncurry Foo) case _ of
  Foo n m -> Just (Tuple n m)
  _ -> Nothing

_Bar :: Prism' FooBar (Tuple3 Boolean String String)
_Bar = prism' (uncurry3 Bar) case _ of
  Bar b s z -> Just (tuple3 b s z)
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
    <|> Bar <$> (lit "bar" *> bool) <*> (param "bar") <*> (param "baz")
    <|> Quux <$> (lit "" *> lit "quux" *> int)
    -- Order matters here.  `list` is greedy, and `end` wont match after it
    <|> End <$> (lit "" *> int <* end)
    <|> Baz <$> (list num <* dir')

bidi :: forall m. Combinators m => MatchClass m => m FooBar
bidi = let discard = C.discard in do
  _Foo  <:> lit "foo" /> num  </> params
  _Bar  <:> lit "bar" /> bool </> param "bar" <&> param "baz"
  _Quux <:> slash /> lit "quux" /> int
  _End  <:> slash /> int </ end
  _Baz  <:> list num </ dir'

testparams :: M.Map String String
testparams = M.fromFoldable
  [ Tuple "bar" "isbar"
  , Tuple "baz" "isbaz"
  ]

testroundtrip :: FooBar -> NonEmpty List String -> Eff (console :: CONSOLE, assert :: ASSERT) Unit
testroundtrip v all@(preferred :| rest) = do
  log $ "Round trip " <> show v
  for_ all \path ->
    let parsed = match bidi path in
    if parsed == Right v
    then log $ "  Parses from " <> path
    else do
      log $ "! ERROR: " <> path <> " parses to " <> show parsed <> "!"
  let produced = build bidi v
  if produced == Right preferred
    then log $ "  Builds back to " <> preferred
    else log $ "! ERROR: Builds " <> show produced <> " instead of " <> preferred <> "!"

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  print "Foo: " $ match bidi "foo/12/?welp='hi'&b=false" -- foo
  print "Quux: " $ match bidi "/quux/42" -- quux
  print "Baz: " $ match bidi "123/" -- baz
  print "End: " $ match bidi "/1" -- end

  let rndtrpBar = match bidi <$> (build bidi $ Bar true "isbar" "isbaz")
  print "Roundtrip Bar: " $ rndtrpBar
  assert $ rndtrpBar == Right (Right $ Bar true "isbar" "isbaz")
  testroundtrip (Foo 3.1 testparams) $ "foo/3.1/?bar=isbar&baz=isbaz":|Nil
  testroundtrip (Baz (1.2:12.0:3.0:Nil)) $ "1.2/12.0/3.0":|"1.2/12.0/3.0/":Nil

  where
    print :: forall e. Show e => String -> e -> Eff (console :: CONSOLE, assert :: ASSERT) Unit
    print s e = log $ append s $ show e

  -- (minimal test for browser)

  -- matches routing $ \old new -> void do
  --   logShow old
  --   logShow new
