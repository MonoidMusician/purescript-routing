module Routing.Match where

import Prelude hiding (apply)

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2, applyFirst, applySecond)
import Control.Plus (class Plus, empty)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Lens.SemiIso (apply)
import Data.List (List(..), reverse)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semiring.Free (Free, free)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Validation.Semiring (V, invalid, unV)
import Global (readFloat, isNaN)
import Routing.Combinators (class Combinators, optional)
import Routing.Match.Class (class MatchClass)
import Routing.Match.Error (MatchError(..), showMatchError)
import Routing.Types (Route, RoutePart(..))

newtype Match a = Match (Route -> V (Free MatchError) (Tuple Route a))

-- Manual instance due to the `Route` synonym in the above
instance newtypeMatch :: Newtype (Match a) (List RoutePart -> V (Free MatchError) (Tuple (List RoutePart) a)) where
  wrap = Match
  unwrap (Match m) = m

instance matchMatchClass :: MatchClass Match where
  lit input = Match \route ->
    case route of
      Cons (Path i) rs | i == input ->
        pure $ Tuple rs unit
      Cons (Path _) rs ->
        invalid $ free $  UnexpectedPath input
      _ ->
        invalid $ free ExpectedPathPart

  num = Match \route ->
    case route of
      Cons (Path input) rs ->
        let res = readFloat input in
        if isNaN res then
          invalid $ free ExpectedNumber
        else
          pure $ Tuple rs res
      _ ->
        invalid $ free ExpectedNumber

  int = Match \route ->
    case route of
      Cons (Path input) rs -> case fromString input of
        Nothing -> invalid $ free ExpectedInt
        Just res -> pure $ Tuple rs res
      _ ->
        invalid $ free ExpectedInt

  bool = Match \route ->
    case route of
      Cons (Path input) rs | input == "true" ->
        pure $ Tuple rs true
      Cons (Path input) rs | input == "false" ->
        pure $ Tuple rs false
      _ ->
        invalid $ free ExpectedBoolean

  str = Match \route ->
    case route of
      Cons (Path input) rs ->
        pure $ Tuple rs input
      _ ->
        invalid $ free ExpectedString

  param key = Match \route ->
    case route of
      Cons (Query map) rs ->
        case M.lookup key map of
          Nothing ->
            invalid $ free $ KeyNotFound key
          Just el ->
            pure $ Tuple (Cons (Query <<< M.delete key $ map) rs) el
      _ ->
        invalid $ free ExpectedQuery

  params = Match \route ->
    case route of
      Cons (Query map) rs ->
        pure $ Tuple rs map
      _ ->
        invalid $ free ExpectedQuery

  end = Match \route ->
    case route of
      Nil -> pure $ Tuple Nil unit
      _ -> invalid $ free ExpectedEnd

  fail msg = Match \_ ->
    invalid $ free $ Fail msg

instance matchFunctor :: Functor Match where
  map fn (Match r2e) = Match $ \r ->
    unV invalid (\(Tuple rs a) -> pure $ Tuple rs (fn a)) $ r2e r

instance matchAlt :: Alt Match where
  alt (Match r2e1) (Match r2e2) = Match $ \r -> do
    (r2e1 r) <|> (r2e2 r)

instance matchPlus :: Plus Match where
  empty = Match $ const $ invalid one

instance matchAlternative :: Alternative Match

instance matchApply :: Apply Match where
  apply (Match r2a2b) (Match r2a) =
    Match \r -> unV (processFnErr r) processFnRes (r2a2b r)
    where processFnErr r err =
            invalid $ err * unV id (const one) (r2a r)
          processFnRes (Tuple rs a2b) =
            map a2b <$> r2a rs

instance matchApplicative :: Applicative Match where
  pure a = Match \r -> pure $ Tuple r a

instance combinatorsMatch :: Combinators Match where
  emptyFail = empty
  emptySuccess = Match (pure <<< (Tuple <@> unit))
  allowed f = optional f
  eitherOr = (<|>)
  semiMap p (Match r2a) = Match \r -> unV invalid (uncurry runiso) (r2a r)
    where runiso rs = apply p >>> either (invalid <<< free <<< Fail) (pure <<< Tuple rs)
  withCurry = lift2 Tuple
  andThen = applySecond
  before = applyFirst

  -- | Matches list of matchers. Useful when argument can easy fail (not `str`)
  -- | returns `Match Nil` if no matches
  list (Match r2a) =
    Match $ go Nil
    where
      go accum r =
        unV
        (const $ pure (Tuple r (reverse accum)))
        (\(Tuple rs a) -> go (Cons a accum) rs)
        (r2a r)



-- It groups `Free MatchError` -> [[MatchError]] -map with showMatchError ->
-- [[String]] -fold with semicolon-> [String] -fold with newline-> String
runMatch :: forall a. Match a -> Route -> Either String a
runMatch (Match fn) route =
  unV foldErrors (Right <<< snd) $ fn route
  where
  foldErrors errs =
    Left $ foldl (\b a -> a <> "\n" <> b) "" do
      es <- reverse <$> unwrap errs
      pure $ foldl (\b a -> a <> ";" <> b) "" $ showMatchError <$>  es


-- | if we match something that can fail then we have to
-- | match `Either a b`. This function converts matching on such
-- | sum to matching on right subpart. Matching on left branch fails.
-- | i.e.
-- | ```purescript
-- | data Sort = Asc | Desc
-- | sortOfString :: String -> Either String Sort
-- | sortOfString "asc" = Right Asc
-- | sortOfString "desc" = Right Desc
-- | sortOfString _ = Left "incorrect sort"
-- |
-- | newtype Routing = Routing Sort
-- | routes :: Match Routing
-- | routes = (pure Routing) <*> (eitherMatch (sortOfString <$> var))
-- |
-- | ```
eitherMatch :: forall a b. Match (Either a b) -> Match b
eitherMatch (Match r2eab) = Match $ \r ->
  unV invalid runEither $ (r2eab r)
  where
  runEither (Tuple rs eit) =
    case eit of
      Left _ -> invalid $ free $ Fail "Nested check failed"
      Right res -> pure $ Tuple rs res
