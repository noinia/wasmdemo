module EffWeb.Html where

import           Data.Coerce
import qualified Data.Dependent.Sum as DSum
import           Data.Foldable
import qualified Data.Foldable as F
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           EffWeb.DOM.Effect
import           EffWeb.DOM.FFI
import           EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))
import           EffWeb.DOM.FFI.Types
import           EffWeb.Html.Attribute
import qualified EffWeb.Html.Attribute as A
import           EffWeb.Html.Element
import           EffWeb.Html.Event
import           EffWeb.Html.Type
import           EffWeb.JSIO
import           EffWeb.Send
import           Effectful

--------------------------------------------------------------------------------

-- | Helper data type moddeling assignments to events or attributes.
data Attr (el :: HtmlElement) (msg :: Type) =           !EventAttr            :- msg
                                            | forall a. !(HtmlAttribute el a) := a

infixr 1 :=, :-

--------------------------------------------------------------------------------

-- Creates a text node
textNode   :: Text -> Html () msg
textNode t = TextNode t mempty

htmlElement            :: forall el msg. ()
                       => HtmlElement
                       -> [Attr el msg]
                       -> [Html () msg]
                       -> Html () msg
htmlElement el ats chs = HtmlNode el mempty
                                     (Map.fromList  [(k,v) | k :- v <- ats])
                                     (attrsFromList [k DSum.:=> Identity v | k := v <- ats])
                                     (Seq.fromList chs)

--------------------------------------------------------------------------------
-- * Some html elements


div :: [Attr Div msg] -> [Html () msg] -> Html () msg
div = htmlElement @Div Div

p :: [Attr P msg] -> [Html () msg] -> Html () msg
p = htmlElement @P P

h1 :: [Attr H1 msg] -> [Html () msg] -> Html () msg
h1 = htmlElement @H1 H1

script :: [Attr Script msg] -> [Html () msg] -> Html () msg
script = htmlElement @Script Script

-- scriptSrc     :: Source -> Html () msg
-- scriptSrc src = script [Src =: src] []

--------------------------------------------------------------------------------
-- * Convenience functions

-- | Renders classes
classes :: Foldable f => f CssClass -> CssClass
classes = CssClass . Text.unwords . map coerce . F.toList
