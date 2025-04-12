-- Module representing how to specify attributes in the view
module EffWeb.Attr
  ( Attr
  , CreateStaticAttr(..)
  , CreateMessageAttr(..)
  , classes
  ) where

import           Data.Coerce
import qualified Data.Dependent.Sum as DSum
import qualified Data.Foldable as F
import qualified Data.Text as Text
import           EffWeb.Html.Attribute
import           EffWeb.Html.Element
import           EffWeb.Varying

--------------------------------------------------------------------------------

-- | Type representing a specification of an attribute.
type Attr model msg = DSum (HtmlAttribute msg) (Varying model)

class CreateStaticAttr attr where
  -- | Create a static attribute
  (=:) :: attr value     -> value -> Attr model msg
  -- DSum (HtmlAttribute msg) Identity
class CreateMessageAttr attr msg where
  -- | Create a message attribute
  (-:) :: attr value -> value -> Attr model msg
  -- DSum (HtmlAttribute msg) Identity

infixr 1 =:, -:

instance CreateStaticAttr GlobalAttribute where
  attr =: value = (GlobalAttribute attr) DSum.:=> Constant value
instance CreateStaticAttr AriaAttribute where
  attr =: value = (AriaAttribute attr) DSum.:=> Constant value

-- instance CreateStaticAttr (HtmlAttribute msg) where
--   attr =: value = attr DSum.:=> Identity value

instance CreateMessageAttr (EventAttr msg) msg where
  attr -: value = (EventAttribute attr) DSum.:=> Constant value

-- instance CreateMessageAttr (HtmlAttribute msg) a where
--   attr -: value = attr DSum.:=> Identity value

--------------------------------------------------------------------------------

-- | Renders classes
classes :: Foldable f => f CssClass -> CssClass
classes = CssClass . Text.unwords . map coerce . F.toList
