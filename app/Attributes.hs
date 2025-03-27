{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Attributes
  ( HtmlAttribute(..)
  , Attributes(..)

  , AttrF
  , HtmlAttr
  , attrsFromList


  , attrNameOf
  , traverseAttributes_

  , HasTextRender(..)

  , DSum

  , Has', has', Has(..)
  , Identity(..)

  , CssClass(..)
  ) where


import           Data.Constraint.Extras
-- import           Data.Dependent.Map (DMap, fromList, singleton, union, unionWithKey)
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum (DSum, (==>))
import           Data.Functor.Identity (Identity(..))
import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.Kind (Type)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.TypeLits
import           HtmlElement (HtmlElement)
import           FFI (HasSetAttributeValue(..))

--------------------------------------------------------------------------------

-- | Maybe used on all elements
data HtmlAttribute el a where
  Accesskey             ::         HtmlAttribute el Text
  Anchor                ::         HtmlAttribute el Text
    -- Experimental Non-standard
  Autocapitalize        ::         HtmlAttribute el Text
  Autocorrect           ::         HtmlAttribute el Text
  Autofocus             ::         HtmlAttribute el Text
  Class                 ::         HtmlAttribute el CssClass
  Contenteditable       ::         HtmlAttribute el Text
  XData                 :: Text -> HtmlAttribute el Text
  Dir                   ::         HtmlAttribute el Text
  Draggable             ::         HtmlAttribute el Bool
  Enterkeyhint          ::         HtmlAttribute el Text
  Exportparts           ::         HtmlAttribute el Text
  Hidden                ::         HtmlAttribute el Text
  Id                    ::         HtmlAttribute el HtmlId
  Inert                 ::         HtmlAttribute el Text
  Inputmode             ::         HtmlAttribute el Text
  Is                    ::         HtmlAttribute el Text
  Itemid                ::         HtmlAttribute el Text
  Itemprop              ::         HtmlAttribute el Text
  Itemref               ::         HtmlAttribute el Text
  Itemscope             ::         HtmlAttribute el Text
  Itemtype              ::         HtmlAttribute el Text
  Lang                  ::         HtmlAttribute el Text
  Nonce                 ::         HtmlAttribute el Text
  Part                  ::         HtmlAttribute el Text
  Popover               ::         HtmlAttribute el Text
  Slot                  ::         HtmlAttribute el Text
  Spellcheck            ::         HtmlAttribute el Text
  Style                 ::         HtmlAttribute el Text
  Tabindex              ::         HtmlAttribute el Text
  Title                 ::         HtmlAttribute el Text
  Translate             ::         HtmlAttribute el Text
  Virtualkeyboardpolicy ::         HtmlAttribute el Text
  -- Experimental
  Writingsuggestions    ::         HtmlAttribute el Text

deriving stock instance Show (HtmlAttribute el a)


instance GShow (HtmlAttribute el) where gshowsPrec = defaultGshowsPrec

instance GEq   (HtmlAttribute el) where geq = defaultGeq

instance GCompare  (HtmlAttribute el) where
  gcompare _ _ = GGT -- FIXME !!

instance ( c Text
         , c HtmlId
         , c CssClass
         , c Bool
         ) => Has c (HtmlAttribute el) where
  -- has forall (a :: k) r. f a -> (c a => r) -> r
  has a x = case a of
    Accesskey              -> x
    Anchor                 -> x
    Autocapitalize         -> x
    Autocorrect            -> x
    Autofocus              -> x
    Class                  -> x
    Contenteditable        -> x
    XData _                -> x
    Dir                    -> x
    Draggable              -> x
    Enterkeyhint           -> x
    Exportparts            -> x
    Hidden                 -> x
    Id                     -> x
    Inert                  -> x
    Inputmode              -> x
    Is                     -> x
    Itemid                 -> x
    Itemprop               -> x
    Itemref                -> x
    Itemscope              -> x
    Itemtype               -> x
    Lang                   -> x
    Nonce                  -> x
    Part                   -> x
    Popover                -> x
    Slot                   -> x
    Spellcheck             -> x
    Style                  -> x
    Tabindex               -> x
    Title                  -> x
    Translate              -> x
    Virtualkeyboardpolicy  -> x
    Writingsuggestions     -> x

--------------------------------------------------------------------------------

-- | Get the Name of a HtmlAttribute
attrNameOf :: HtmlAttribute el a -> Text
attrNameOf = \case
  Accesskey             -> "accesskey"
  Anchor                -> "anchor"
  Autocapitalize        -> "autocapitalize"
  Autocorrect           -> "autocorrect"
  Autofocus             -> "autofocus"
  Class                 -> "class"
  Contenteditable       -> "contenteditable"
  XData label           -> "data-" <> label
  Dir                   -> "dir"
  Draggable             -> "draggable"
  Enterkeyhint          -> "enterkeyhint"
  Exportparts           -> "exportparts"
  Hidden                -> "hidden"
  Id                    -> "id"
  Inert                 -> "inert"
  Inputmode             -> "inputmode"
  Is                    -> "is"
  Itemid                -> "itemid"
  Itemprop              -> "itemprop"
  Itemref               -> "itemref"
  Itemscope             -> "itemscope"
  Itemtype              -> "itemtype"
  Lang                  -> "lang"
  Nonce                 -> "nonce"
  Part                  -> "part"
  Popover               -> "popover"
  Slot                  -> "slot"
  Spellcheck            -> "spellcheck"
  Style                 -> "style"
  Tabindex              -> "tabindex"
  Title                 -> "title"
  Translate             -> "translate"
  Virtualkeyboardpolicy -> "virtualkeyboardpolicy"
  Writingsuggestions    -> "writingsuggestions"


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

-- data AttributeKind a = Global   (GlobalAttribute a)
--                      | Specific (SpecificAttribute a)
--                      deriving (Show)

-- instance GShow    AttributeKind where gshowsPrec = showsPrec
-- instance GEq      AttributeKind where geq = defaultGeq
-- instance GCompare AttributeKind where
--   gcompare (Global g)   (Global g')   = gcompare g g'
--   gcompare (Global g)   (Specific g') = GLT
--   gcompare (Specific g) (Specific g') = gcompare g g'
--   gcompare (Specific g) (Global g')   = GGT

-- instance (Has c GlobalAttribute, Has c SpecificAttribute) => Has c AttributeKind where
--   -- has forall (a :: k) r. f a -> (c a => r) -> r
--   has a x = case a of
--     Global   a' -> has @c a' x
--     Specific a' -> has @c a' x


--------------------------------------------------------------------------------

newtype Attributes el = Attributes (DMap.DMap (HtmlAttribute el) Identity)
  deriving (Show)


type AttrF el'   = DSum el' Identity

type HtmlAttr el = AttrF (HtmlAttribute el)

attrsFromList :: [HtmlAttr el] -> Attributes el
attrsFromList = Attributes . DMap.fromList


class HasJSFFI a where
instance HasJSFFI Int
instance HasJSFFI Bool


-- data JSSerialized a where
--   AsBool :: JSSerialized Int


-- class JSSerializable el a where
--   serializeJS :: HtmlAttribute el a -> JSSerialized a



class HasTextRender a where
  renderAsText :: a -> Text

instance HasTextRender a => HasTextRender (Identity a) where
  renderAsText (Identity x) = renderAsText x
instance HasTextRender Text where
  renderAsText = id

-- | Traversa over the attributes
traverseAttributes_                  :: ( Applicative t
                                        )
                                     => (forall a. HtmlAttribute el a -> a -> t ())
                                     -> Attributes el -> t ()
traverseAttributes_ f (Attributes m) = DMap.traverseWithKey_ (\attr (Identity x) -> f attr x) m


newtype HtmlId = HtmlId Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)


-- type family AttributeValue (attr :: AttributeKind) :: Type

-- type instance AttributeValue (Global Id)    = HtmlId
-- type instance AttributeValue (Global Title) = Text


--------------------------------------------------------------------------------

newtype CssClass = CssClass Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)


-- data Style = Style
