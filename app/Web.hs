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
import           Data.Monoid (First(..))
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
import           EffWeb.Html.Element
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
main = runEff . evalJSIO . evalDOM $ do
         body <- jsBody
         let handlerSetup :: Eff [DOM,JSIO,IOE] () -> IO ()
             handlerSetup = runEff . evalJSIO . evalDOM
         _    <- runReader myModel $ renderView handlerSetup body myUI
         pure ()


--------------------------------------------------------------------------------

data HtmlBody f ref msg = TextNode ref (f Text)
                        | ElemNode {-#UNPACK #-}!HtmlElement
                                   ref
                                   (AttributesF f msg)
                                   (Seq.Seq     (f (HtmlBody f ref msg)))
                      -- deriving stock (Show,Eq,Functor)

instance Functor f => Functor (HtmlBody f ref) where
  fmap f = \case
    TextNode ref ft         -> TextNode ref ft
    ElemNode el ref ats chs -> ElemNode el ref (fmap f ats) (fmap (fmap (fmap f)) chs)

instance Functor f => Bifunctor (HtmlBody f) where
  bimap f g = \case
    TextNode ref ft         -> TextNode (f ref) ft
    ElemNode el ref ats chs -> ElemNode el (f ref) (fmap g ats) (fmap (fmap (bimap f g)) chs)


--------------------------------------------------------------------------------

----------------------------------------


newtype View' ref model msg = View { unView :: Varying model (HtmlBody (Varying model) ref msg) }
  deriving stock (Functor)

mapRef            :: (ref -> ref') -> View' ref model msg -> View' ref' model msg
mapRef f (View v) = View . fmap (first f) $ v

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
createHtmlElement              :: HtmlElement
                               -> Varying model (Attributes model msg)
                               -> Varying model (Seq.Seq (View model msg))
                               -> View model msg
createHtmlElement el mats mchs = View $ ElemNode el mempty <$> coerce mats <*> coerce mchs

-- | More or less the same as createHtmlElement, but in an easier to use form.
dynHtmlElement            :: HtmlElement
                          -> Varying model [Attr model msg]
                          -> Varying model [View model msg]
                          -> View model msg
dynHtmlElement el ats chs = createHtmlElement el (attrsFromList <$> ats) (Seq.fromList <$> chs)

-- | Create a html element with a fixed set of attributes, and a fixed set of children,
-- however those attributes/children themselves may vary over time.
htmlElement            :: HtmlElement -> [Attr model msg] -> [View model msg] -> View model msg
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


data RenderState model = RenderState { elemRef  :: Maybe Element
                                       -- should this be a HKD so we can guarantee there is
                                       -- an elem?
                                     , oldModel :: Maybe model
                                     -- ^ model used at the time of construction (if relevant)

                                     -- I'm not quite sure that this is a good idea yet,
                                     -- as it retains old models. Moreover, all elements
                                     -- could retain some other model; so that seems expensive.
                                     }

-- | Initial rendering state
initialState :: RenderState model
initialState = RenderState Nothing Nothing



data ShouldRender = NoUpdate | Create | Update Element

-- | Test whether we should (re)render the part of the tree
shouldRender     :: RenderState model -> Varying model' a -> ShouldRender
shouldRender rs v = case elemRef rs of
                      Nothing  -> Create
                      Just ref -> case v of
                        Constant _ -> NoUpdate
                        Varying _  -> Update ref


-- | Acquires the model meeded for a 'varying' from the context. Returns the model if
-- used.
acquire :: Reader model :> es => Varying model a -> Eff es (a, Maybe model)
acquire = \case
  Constant x -> pure (x, Nothing)
  Varying f  -> (\model -> (f model, Just model)) <$> ask


-- | Renders a view
renderView                     :: forall root ref es model msg handlerEs.
                                  ( IsNode root
                                  , DOM :> es
                                  , Reader model :> es
                           , JSIO :> handlerEs
                                  )
                               => (Eff handlerEs () -> IO ())
                               -- ^ the environment in which we evaluate a event handler
                               -> root -> View' ref model msg
                               -> Eff es (View' (RenderState model) model msg)
renderView handlerSetup root v = let View var = mapRef (const initialState) v in
    View <$> runCanRunHandler handlerSetup (createHtmlVarying @handlerEs root var)


-- | Helper to create a varying
createVaryingWith   :: (Reader model :> es)
                    => (a -> Eff es a)
                    -> Varying model a -> Eff es (Varying model a)
createVaryingWith f = \case
  Constant x -> Constant <$> f x
  Varying g  -> do x   <- asks g
                   res <- f x
                   pure $ Varying g
  -- hmm, this throws away the result; that also doesn't sound right

-- | Create a html subtree that may vary depending on the model
createHtmlVarying       :: forall handlerEs es root model msg.
                           ( IsNode root
                           , DOM                     :> es
                           , Reader model            :> es
                           , CanRunHandler handlerEs :> es
                           , JSIO :> handlerEs
                           )
                         => root
                         -> Varying model (HtmlBody (Varying model) (RenderState model) msg)
                         -> Eff es (Varying model (HtmlBody (Varying model) (RenderState model) msg))
createHtmlVarying parent = createVaryingWith (createHtml' @handlerEs parent)

-- | Creates the html tree
createHtml'             :: forall handlerEs root model msg es.
                           ( IsNode root
                           , DOM                     :> es
                           , Reader        model     :> es
                           , CanRunHandler handlerEs :> es
                           , JSIO :> handlerEs
                           )
                        => root
                        -> HtmlBody (Varying model) (RenderState model) msg
                        -> Eff es (HtmlBody (Varying model) (RenderState model) msg)
createHtml' parent body = case body of
    TextNode rs vText -> case shouldRender rs vText of
      NoUpdate       -> pure body
      Create         -> do (text, mModel) <- acquire vText
                           textRef <- coerce <$> createTextNode text
                           appendChild parent textRef
                           pure $ TextNode (rs { elemRef  = Just textRef
                                               , oldModel = mModel
                                               }
                                           ) vText
      Update textRef -> do (text, mModel) <- acquire vText
                           -- TODO: set the text
                           pure $ TextNode (rs { oldModel = mModel} ) vText
    ElemNode el rs attrs chs -> case elemRef rs of
      Nothing       -> do elRef <- createElement (elementNameOf el)
                          appendChild parent elRef
                          -- create the attributes

                          model <- ask -- TODO fix
                          traverseAttributes_ model (setAttribute' elRef) (Attributes attrs)
                          chs'      <- traverse (createHtmlVarying @handlerEs elRef) chs
                            -- TODO: maintain whether we access the model or not
                          let mModel = Just model
                            -- First mModel = attrModel -- <> chsModel
                              -- figure out whether this subtree actually needed the model.
                          pure $ ElemNode el (rs { elemRef  = Just elRef
                                                 , oldModel = mModel
                                                 }
                                             ) attrs chs'
      Just elRef    -> pure body -- FIXME  -- maybe update
  where
    setAttribute'                   :: Element -> HtmlAttribute msg v -> v
                                    -> Eff es ()
    setAttribute' elRef attr value = case attr of
      GlobalAttribute attr'    -> has @HasSetAttributeValue attr' setAttribute elRef attr value
      AriaAttribute attr'      -> has @HasSetAttributeValue attr' setAttribute elRef attr value
      EventAttribute eventAttr -> addEventListener elRef eventAttr (asHandler eventAttr value)

    asHandler              :: EventAttr msg a -> _ -> Event -> Eff handlerEs ()
    asHandler _ _ rawEvent = consoleLog "fired"
      --

      -- do
      --     (value, _) <- acquire vValue
      --     has @HasSetAttributeValue attr setAttribute elRef attr value
      --     -- pure $ First mModel








--   = \case
--     TextNode _ vtext ->



    -- text              -> do txtRef <- createTextNode text
    --                                    appendChild parent txtRef
    --                                    pure $ TextNode (coerce txtRef) text  -- TODO
    -- HtmlNode el _ evts attrs chs -> do elRef <- createElement (elementNameOf el)
    --                                    appendChild parent elRef
    --                                    -- set attrs
    --                                    -- setAttribute elRef Id "foo"
    --                                    -- traverseAttributes_ (\attr value ->
    --                                    --   has @HasSetAttributeValue attr
    --                                    --      setAttribute elRef attr value) attrs

    --                                    -- register event handles
    --                                    registerEventHandles handler elRef evts

    --                                    chs' <- traverse (createHtml @handlerEs elRef) chs
    --                                    pure $ HtmlNode el elRef evts attrs chs'



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
