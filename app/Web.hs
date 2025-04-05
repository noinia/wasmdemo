{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import           Control.Monad (void, (=<<))
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Coerce
import qualified Data.Dependent.Sum as DSum
import           Data.Foldable
import qualified Data.Foldable as F
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Profunctor
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           EffWeb.DOM.Effect
import           EffWeb.DOM.FFI
import           EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))
import           EffWeb.DOM.FFI.Types
import           EffWeb.Html.Attribute
import           EffWeb.JSIO
import           EffWeb.Varying
import           Effectful
import           Effectful.Concurrent.STM
import           Effectful.Dispatch.Dynamic
import qualified Effectful.Dispatch.Dynamic as Eff
import           Effectful.Dispatch.Static
import           Effectful.Dispatch.Static.Primitive (emptyEnv)
import           Effectful.Reader.Static
import           GHC.Wasm.Prim
import           Prelude hiding (div)

--------------------------------------------------------------------------------

-- type JSFunction = JSVal

foreign export javascript "hs_start"
  main :: IO ()

--------------------------------------------------------------------------------

main :: IO ()
main = print "woei"


--------------------------------------------------------------------------------

data HtmlElem = Div | P | H1
  deriving (Show,Eq)

-- data Attr msg = OnClick' msg
--               | Class' Text
--               deriving (Show,Eq,Functor)

data HtmlBody f ref msg = TextNode ref (f Text)
                        | ElemNode {-#UNPACK #-}!HtmlElem
                                   ref
                                   (AttributesF f msg)
                                   (Seq.Seq     (f (HtmlBody f ref msg)))
                      -- deriving stock (Show,Eq,Functor)

instance Functor f => Functor (HtmlBody f ref) where
  fmap f = \case
    TextNode ref ft         -> TextNode ref ft
    ElemNode el ref ats chs -> ElemNode el ref (fmap f ats) (fmap (fmap (fmap f)) chs)

--------------------------------------------------------------------------------

----------------------------------------


newtype View' ref model msg = View { unView :: Varying model (HtmlBody (Varying model) ref msg) }
  deriving stock (Functor)




type View = View' ()

instance Profunctor (View' ref) where
  dimap                  :: forall model model' msg msg'.
                            (model' -> model)
                         -> (msg   -> msg')
                         -> View' ref model msg -> View' ref model' msg'
  dimap f g (View vBody) = View $ dimap f dimapHtml vBody
    where
      dimapHtml :: HtmlBody (Varying model) ref msg -> HtmlBody (Varying model') ref msg'
      dimapHtml = \case
        TextNode ref vt         -> TextNode ref (lmap f vt)
        ElemNode el ref ats chs -> ElemNode el ref (coerce $ dimap f g $ Attributes ats)
                                                   (fmap (dimap f dimapHtml) chs)

--------------------------------------------------------------------------------

dynText :: Varying model Text -> View model msg
dynText = View . pure . TextNode ()

textNode :: (model -> Text) -> View model msg
textNode = dynText . Varying

staticText :: Text -> View model msg
staticText = View . pure . TextNode mempty . pure

----------------------------------------

-- | The most generic version of create htmlElement, that allows us to change
-- which attributes and which children exist over time.
createHtmlElement              :: HtmlElem
                               -> Varying model (Attributes model msg)
                               -> Varying model (Seq.Seq (View model msg))
                               -> View model msg
createHtmlElement el mats mchs = View $ ElemNode el mempty <$> coerce mats <*> coerce mchs

-- | More or less the same as createHtmlElement, but in an easier to use form.
dynHtmlElement            :: HtmlElem
                          -> Varying model [Attr model msg]
                          -> Varying model [View model msg]
                          -> View model msg
dynHtmlElement el ats chs = createHtmlElement el (attrsFromList <$> ats) (Seq.fromList <$> chs)

-- | Create a html element with a fixed set of attributes, and a fixed set of children,
-- however those attributes/children themselves may vary over time.
htmlElement            :: HtmlElem -> [Attr model msg] -> [View model msg] -> View model msg
htmlElement el ats chs = dynHtmlElement el (pure ats) (pure chs)

div :: [Attr model msg] -> [View model msg] -> View model msg
div = htmlElement Div

p :: [Attr model msg] -> [View model msg] -> View model msg
p = htmlElement P

h1 :: [Attr model msg] -> [View model msg] -> View model msg
h1 = htmlElement H1

-- | Renders classes
classes :: Foldable f => f CssClass -> CssClass
classes = CssClass . Text.unwords . map coerce . F.toList

--------------------------------------------------------------------------------

-- data Attr msg

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

data MyModel = MyModel { myBooleanValue :: Bool
                       , modelText ::      Text
                       }
  deriving stock (Show,Eq)

myModel :: MyModel
myModel = MyModel False "initial model"

data MyMsg = HasBeenClicked
           | SetMsg Text
           | MyInitialAction


myUI :: View MyModel MyMsg
myUI = div []
           [ h1  [ Class   =: classes ["header", "someclass"]
                 , OnClick -: HasBeenClicked
                 , Id      =: "theHeader"
                 ]
                 [ staticText "header!"
                 ]
           , div [] [p [ OnClick     -: SetMsg "woei"
                       , XData "foo" =: "bar"
                       , Style       =: "border: 1px solid black; width: 200px; height: 100px;"
                       , OnMouseOver -: \_ -> SetMsg "hovering"
                       ]
                       [ textNode modelText
                       ]
                    ]
           ]




{-

     $ do consoleLog "woei"
          body   <- jsBody

          tr <- createHtml body (myUI myModel)

          textNode <- createTextNode "my text on load"
          appendChild body textNode
          -- onLoad $ do
          --   consoleLog "loaded"
          --   body   <- jsBody
          --   textNode <- createTextNode "my text on load"
          --   appendChild body textNode

          -- onClick $ do
          --   consoleLog "clicked"
          --   body   <- jsBody
          --   theDiv <- createElement "div"

          --   appendChild body theDiv

          --   textNode <- createTextNode "my text node :) "
          --   appendChild body textNode

          consoleLog "added"

-}


--------------------------------------------------------------------------------

showT :: Show a => a -> Text
showT = Text.pack . show
