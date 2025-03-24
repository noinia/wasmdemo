{-# LANGUAGE UndecidableInstances #-}
module Attributes
  ( HtmlAttribute(..)
  , Attributes(..)

  , AttrF
  , HtmlAttr
  , attrsFromList

  , DSum, (==>)
  , Identity(..)
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

--------------------------------------------------------------------------------

-- | Maybe used on all elements
data HtmlAttribute el a where
  Accesskey             ::         HtmlAttribute el Text
  Anchor                ::         HtmlAttribute el Text
    -- Experimental Non-standard
  Autocapitalize        ::         HtmlAttribute el Text
  Autocorrect           ::         HtmlAttribute el Text
  Autofocus             ::         HtmlAttribute el Text
  Class                 ::         HtmlAttribute el Text
  Contenteditable       ::         HtmlAttribute el Text
  XData                 :: Text -> HtmlAttribute el Text
  Dir                   ::         HtmlAttribute el Text
  Draggable             ::         HtmlAttribute el Text
  Enterkeyhint          ::         HtmlAttribute el Text
  Exportparts           ::         HtmlAttribute el Text
  Hidden                ::         HtmlAttribute el Text
  Id                    ::         HtmlAttribute el Text
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

instance c Text => Has c (HtmlAttribute el) where
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


newtype HtmlId = HtmlId Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)


-- type family AttributeValue (attr :: AttributeKind) :: Type

-- type instance AttributeValue (Global Id)    = HtmlId
-- type instance AttributeValue (Global Title) = Text
