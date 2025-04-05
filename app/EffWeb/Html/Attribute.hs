{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module EffWeb.Html.Attribute
  ( HtmlAttribute(..)

  , AriaAttribute(..)

  , Attributes(..)
  , AttributesF(..)
  , HtmlAttr
  , attrsFromList


  , traverseAttributes_

  , module EffWeb.Html.Attribute.Common
  , module EffWeb.Html.Attribute.Global
  , module EffWeb.Html.Event

  , DSum

  , Has', has', Has(..)
  , Identity(..)

  , CssClass(..)
  ) where

import           Data.Coerce
import           Data.Constraint.Extras
import           Data.Profunctor
import           EffWeb.Varying
-- import           Data.Dependent.Map (DMap, fromList, singleton, union, unionWithKey)
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum (DSum(..), (==>))
import           Data.Functor.Identity (Identity(..))
import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.Kind (Type)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.TypeLits
import           EffWeb.Html.Element (HtmlElement)
import           EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))

import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare.TH (deriveGEq,deriveGCompare)
import           Data.GADT.Show.TH (deriveGShow)
import           EffWeb.Html.Attribute.Common
import           EffWeb.Html.Attribute.Global
import           EffWeb.Html.Event

--------------------------------------------------------------------------------


data AriaAttribute a where
  Aria :: !Text -> AriaAttribute Text
  -- TODO

instance HasAttrName (AriaAttribute a) where
  attrNameOf (Aria t) = "aria-" <> t


deriving instance Show a => Show (AriaAttribute a)

deriveGEq      ''AriaAttribute
deriveGCompare ''AriaAttribute
deriveGShow    ''AriaAttribute
deriveArgDict  ''AriaAttribute


data HtmlAttribute msg a = GlobalAttribute (GlobalAttribute a)
                         | AriaAttribute   (AriaAttribute a)
                         | EventAttribute  (EventAttr msg a)
                         deriving (Show)

-- | Change the message type
mapAttr                   :: Functor f
                          => (msg -> msg')
                          -> DSum (HtmlAttribute msg) f -> DSum (HtmlAttribute msg') f
mapAttr f (attr :=> fval) = case attr of
                              GlobalAttribute a -> GlobalAttribute a          :=> fval
                              AriaAttribute   a -> AriaAttribute   a          :=> fval
                              EventAttribute  a -> case mapEvent f (a :=> fval) of
                                                     a' :=> fval' -> EventAttribute a' :=> fval'

mapAttrWith               :: Functor f
                          => (forall v. f v -> g v)
                          -> (msg -> msg')
                          -> DSum (HtmlAttribute msg) f -> DSum (HtmlAttribute msg') g
mapAttrWith ff f (attr :=> fval) = case attr of
    GlobalAttribute a -> GlobalAttribute a          :=> ff fval
    AriaAttribute   a -> AriaAttribute   a          :=> ff fval
    EventAttribute  a -> case mapEvent f (a :=> fval) of
                           a' :=> fval' -> EventAttribute a' :=> ff fval'

instance GEq      (HtmlAttribute msg) where geq = defaultGeq
instance GCompare (HtmlAttribute msg) where
  gcompare (GlobalAttribute a) (GlobalAttribute a') = gcompare a a'
  gcompare (GlobalAttribute a) _                    = GLT

  gcompare (AriaAttribute _)   (GlobalAttribute a') = GGT
  gcompare (AriaAttribute a)   (AriaAttribute a')   = gcompare a a'
  gcompare (AriaAttribute _)   (EventAttribute _)   = GLT

  gcompare (EventAttribute a)  (EventAttribute a')  = gcompare a a'
  gcompare (EventAttribute _)  _                    = GGT

-- instance GShow (HtmlAttribute msg) where gshowsPrec = defaultGshowsPrec

instance ( Has c GlobalAttribute, Has c AriaAttribute, Has c (EventAttr msg)
         ) => Has c (HtmlAttribute msg) where
  has a x = case a of
    GlobalAttribute ga -> has @c ga x
    AriaAttribute aa   -> has @c aa x
    EventAttribute ea  -> has @c ea x

instance HasAttrName (HtmlAttribute msg a) where
  attrNameOf = \case
    GlobalAttribute ga -> attrNameOf ga
    AriaAttribute aa   -> attrNameOf aa
    EventAttribute ea  -> attrNameOf ea

--------------------------------------------------------------------------------



-- --------------------------------------------------------------------------------
-- data SpecificAttribute a where
--   TODO :: SpecificAttribute Text

-- deriving instance (Show a) => Show (SpecificAttribute a)

-- instance GShow    SpecificAttribute where gshowsPrec = showsPrec
-- instance GEq      SpecificAttribute where geq = defaultGeq
-- instance GCompare SpecificAttribute where
--   gcompare _ _ = GEQ

-- instance Has c SpecificAttribute where
--   -- has forall (a :: k) r. f a -> (c a => r) -> r
--   has a x = case a of
--     TODO -> x

-- -- deriveGEq      ''SpecificAttribute
-- -- deriveGCompare ''SpecificAttribute
-- -- deriveGShow    ''SpecificAttribute
-- -- deriveArgDict  ''SpecificAttribute

-- --------------------------------------------------------------------------------

newtype Attributes model msg = Attributes (AttributesF (Varying model) msg)
  deriving newtype (Functor)

newtype AttributesF f msg = AttributesF (DMap.DMap (HtmlAttribute msg) f)


instance Functor f => Functor (AttributesF f) where
  fmap f (AttributesF m) = AttributesF . DMap.fromAscList . fmap (mapAttr f) . DMap.toAscList $ m
    -- note that changing the message type cannot change the ordering of the key types; as
    -- e.g. we cannot change from something like an 'EventAttribute OnPause' attribute to
    -- a GlobalAttribute or so. Hence the ordering does not change

-- mapAttributes' =

instance Profunctor Attributes where
  -- f :: (model -> model')
  -- g :: (msg -> msg')
  dimap f g (Attributes (AttributesF m)) = Attributes . AttributesF
                                         . DMap.fromAscList . fmap (mapAttrWith (lmap f) g)
                                         . DMap.toAscList $ m

type HtmlAttr model msg = DSum (HtmlAttribute msg) (Varying model)

attrsFromList :: [HtmlAttr model msg] -> Attributes model msg
attrsFromList = Attributes . AttributesF . DMap.fromList

class HasJSFFI a where
instance HasJSFFI Int
instance HasJSFFI Bool


-- data JSSerialized a where
--   AsBool :: JSSerialized Int


-- class JSSerializable el a where
--   serializeJS :: GlobalAttribute el a -> JSSerialized a


-- | Traversal over the attributes
traverseAttributes_               :: forall model msg t. Applicative t
                                  => model
                                  -> (forall a. HtmlAttribute msg a -> a -> t ())
                                  -> Attributes model msg -> t ()
traverseAttributes_ input f attrs = DMap.traverseWithKey_ ff (coerce attrs)
  where
    ff      :: HtmlAttribute msg a -> Varying model a -> t ()
    ff attr = \case
      Constant x -> f attr x
      Varying g  -> f attr (g input)

--------------------------------------------------------------------------------

newtype Source = Source Text
  deriving stock (Show,Eq)
  deriving newtype (IsString)
