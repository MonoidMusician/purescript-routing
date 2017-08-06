module Routing.Generic where

import Data.Array (fromFoldable, toUnfoldable)
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Field(Field), NoArguments(..), NoConstructors, Product(Product), Rec(..), Sum(Inr, Inl), from, to)
import Data.Lens (Iso', Prism', _Just, _Left, _Nothing, _Right, iso, prism', re)
import Data.Lens.Internal.Retail (Retail(..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.SemiIso (ASemiIso', SemiIso', constant, semiIso, weaken)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Record as R
import Data.String (toLower)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant, contract, expand, inj, prj)
import Data.Variant.Internal (class Contractable, class VariantTags)
import Prelude hiding (compose)
import Routing (match)
import Routing.Combinators (class Combinators, emptyFail, emptySuccess, list, (/>), (</>), (<:>), (<=>), (<||>))
import Routing.Match.Class (class MatchClass, bool, int, lit, num, param, params, str)
import Routing.RouteBuilder (build)
import Type.Row (class ListToRow, class RowLacks, class RowToList, Cons, Nil, RLProxy(..), RProxy(..), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

class Routing a where
  routing :: forall c. Combinators c => MatchClass c => c a

_Routing :: forall a. Routing a => SemiIso' (Either String) String a
_Routing = semiIso (match routing) (build routing)

instance routingBoolean :: Routing Boolean where
  routing = bool

instance routingInt :: Routing Int where
  routing = int

instance routingNumber :: Routing Number where
  routing = num

instance routingString :: Routing String where
  routing = str

instance routingMaybe :: Routing a => Routing (Maybe a) where
  routing = _Just <:> routing <||> _Nothing <:> emptySuccess

instance routingEither :: (Routing a, Routing b) => Routing (Either a b) where
  routing = _Left <:> routing <||> _Right <:> routing

newtype Parameter (s :: Symbol) a = Parameter a
derive instance newtypeParameter :: Newtype (Parameter s a) _
derive newtype instance eqParameter :: Eq a => Eq (Parameter s a)

instance routingParameter :: (IsSymbol s, Routing a) => Routing (Parameter s a) where
  routing = answer <=> param (reflectSymbol (SProxy :: SProxy s))
    where
      -- uc :: SemiIso' (Either String) String a -> SemiIso' (Either String) String (Parameter s a)
      -- uc = unsafeCoerce
      -- _R = uc _r :: SemiIso' (Either String) String (Parameter s a)
      _r = _Routing -- :: SemiIso' (Either String) String a
      _nr = weaken (re _Newtype) -- :: SemiIso' (Either String) a (Parameter s a)
      Retail f f' = _nr (Retail pure pure)
      Retail g g' = _r (Retail pure pure)
      f'' = f <=< g
      g'' = g' <=< f'
      --answer :: SemiIso' (Either String) String (Parameter s a)
      answer = semiIso f'' g''
      --answer = compose anr ar

instance routingList :: Routing a => Routing (List a) where
  routing = list routing

instance routingArray :: Routing a => Routing (Array a) where
  routing = iso toUnfoldable fromFoldable <:> (list routing)

instance routingParameters :: Routing (Map String String) where
  routing = params

instance routingVariant :: (RoutingVariant r rl) => Routing (Variant r) where
  routing = routingVariantL (RLProxy :: RLProxy rl)

class (RowToList r rl, ListToRow rl r) <= RoutingVariant (r :: # Type) (rl :: RowList) | rl -> r where
  routingVariantL :: forall c. Combinators c => MatchClass c => RLProxy rl -> c (Variant r)

instance routingVariantNil :: RoutingVariant () Nil where
  routingVariantL _ = emptyFail

_variant ::
  forall s t r r'.
    IsSymbol s =>
    RowCons s t r r' =>
  SProxy s -> Prism' (Variant r') t
_variant s = prism' (inj s) (prj s)
_recast ::
  forall r r' n.
    Union r' n r =>
    Contractable r r' =>
  RProxy n -> Prism' (Variant r) (Variant r')
_recast _ = prism' expand contract
_without ::
  forall s t r r' n.
    RowCons s t r' r =>
    -- TODO: way to infer these constraints?
    RowSingleton s t n =>
    Union r' n r =>
    Contractable r r' =>
  SProxy s -> Prism' (Variant r) (Variant r')
_without _ = _recast (RProxy :: RProxy n)

class RowCons s t () r <= RowSingleton s t r | -> s t r
instance rowSingleton ::
  ( RowToList r (Cons s t Nil)
  , RowCons s t () r
  ) => RowSingleton s t r

class
  ( IsSymbol s
  , RowCons s t r' r
  , RowLacks s r'
  -- causes errors around RowCons s t r' r if included?
  --, RowSingleton s t n
  , Union r' n r
  , RowToList r' rl'
  , ListToRow rl' r'
  , RowToList r rl
  , ListToRow rl r
  ) <= RowListStep s t r' r n rl' rl | -> s t r' r n rl' rl

instance rowListStep ::
  ( IsSymbol s
  , RowCons s t r' r
  , RowLacks s r'
  , RowSingleton s t n
  , Union r' n r
  , RowToList r' rl'
  , ListToRow rl' r'
  , RowToList r (Cons s t rl')
  , ListToRow (Cons s t rl') r
  ) => RowListStep s t r' r n rl' (Cons s t rl')

instance routingVariantCons ::
  ( RowListStep s t r' r n rl' (Cons s t rl')
  , RowSingleton s t n
  , Routing t
  , RoutingVariant r' rl'
  , VariantTags rl'
  ) => RoutingVariant r (Cons s t rl')
  where
    routingVariantL _ = this <||> other
      where
        this = _variant (SProxy :: SProxy s) <:>
          routing
        other = _without SProxy <:>
          routingVariantL (RLProxy :: RLProxy rl')

class (RowToList r rl, ListToRow rl r) <= RoutingRecord (r :: # Type) (rl :: RowList) | rl -> r where
  routingRecordL :: forall c. Combinators c => MatchClass c => RLProxy rl -> c (Record r)

_rowcons ::
  forall s t r r'.
    IsSymbol s =>
    RowCons s t r' r =>
    RowLacks s r' =>
  SProxy s -> Iso' (Record r) (Tuple t (Record r'))
_rowcons s = iso
  (\r -> Tuple (R.get s r) (R.delete s r))
  (\(Tuple val r) -> R.insert s val r)

instance routingRecordNil :: RoutingRecord () Nil where
  routingRecordL _ = constant {} <=> emptySuccess

instance routingRecordCons ::
  ( RowListStep s String r' r n rl' (Cons s String rl')
  , RowLacks s r' -- FIXME: why?? should be covered by RowListStep??
  , RoutingRecord r' rl'
  , Routing String
  ) => RoutingRecord r (Cons s String rl')
  where
    routingRecordL _ = _rowcons s <:> this </> rest
      where
        this = param (reflectSymbol s)
        rest = routingRecordL (RLProxy :: RLProxy rl')
        s = SProxy :: SProxy s

class RoutingGeneric rep where
  routingGenericRep :: forall c. Combinators c => MatchClass c => c rep

transformName :: forall s. IsSymbol s => SProxy s -> String
transformName = reflectSymbol >>> toLower

instance routingGenericRepNoConstructors :: RoutingGeneric NoConstructors where
  routingGenericRep = emptyFail

instance routingGenericRepNoArguments :: RoutingGeneric NoArguments where
  routingGenericRep = constant NoArguments <=> emptySuccess

_Rec :: forall a. Iso' (Rec a) a
_Rec = iso (\(Rec a) -> a) Rec

instance routingGenericRepRec :: RoutingGeneric a => RoutingGeneric (Rec a) where
  routingGenericRep = _Rec <:> routingGenericRep

_Field :: forall name a. Iso' (Field name a) a
_Field = iso (\(Field a) -> a) Field

instance routingGenericRepField ::
  ( RoutingGeneric a
  , IsSymbol name
  ) => RoutingGeneric (Field name a) where
  routingGenericRep = _Field <:> lit path /> routingGenericRep
    where path = transformName (SProxy :: SProxy name)

_Constructor :: forall name a. Iso' (Constructor name a) a
_Constructor = iso (\(Constructor a) -> a) Constructor

instance routingGenericRepConstructor ::
  ( RoutingGeneric a
  , IsSymbol name
  ) => RoutingGeneric (Constructor name a) where
  routingGenericRep = _Constructor <:> lit path /> routingGenericRep
    where path = transformName (SProxy :: SProxy name)

_Argument :: forall a. Iso' (Argument a) a
_Argument = iso (\(Argument a) -> a) Argument

instance routingGenericRepArgument ::
  ( Routing a
  ) => RoutingGeneric (Argument a) where
  routingGenericRep = _Argument <:> routing

_Inl :: forall l r. Prism' (Sum l r) l
_Inl = prism' Inl case _ of
  Inl l -> Just l
  _ -> Nothing

_Inr :: forall l r. Prism' (Sum l r) r
_Inr = prism' Inr case _ of
  Inr r -> Just r
  _ -> Nothing

instance routingSum ::
  ( RoutingGeneric a
  , RoutingGeneric b
  ) => RoutingGeneric (Sum a b)
  where
    routingGenericRep = _Inl <:> routingGenericRep <||> _Inr <:> routingGenericRep

_Product :: forall l r. Iso' (Product l r) (Tuple l r)
_Product = iso
  (\(Product l r) -> Tuple l r)
  (\(Tuple l r) -> Product l r)

instance routingProduct ::
  ( RoutingGeneric a
  , RoutingGeneric b
  ) => RoutingGeneric (Product a b)
  where
    routingGenericRep = _Product <:> routingGenericRep </> routingGenericRep

_Generic :: forall a rep. Generic a rep => Iso' a rep
_Generic = iso from to

routingGeneric :: forall a rep c. Generic a rep => RoutingGeneric rep => Combinators c => MatchClass c => c a
routingGeneric = _Generic <:> routingGenericRep
