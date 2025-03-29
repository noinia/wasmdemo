{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import           Attributes
import           Control.Monad (void)
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
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           Effectful
import           Effectful.Concurrent.STM
import           Effectful.Dispatch.Dynamic
import qualified Effectful.Dispatch.Dynamic as Eff
import           Effectful.Dispatch.Static
import           Effectful.Dispatch.Static.Primitive (emptyEnv)
import           Effectful.Reader.Static
import           FFI
import           FFI.Types
import           GHC.Wasm.Prim
import           HtmlElement
import           HtmlEvent
import           Prelude hiding (div)

--------------------------------------------------------------------------------

-- type JSFunction = JSVal

foreign export javascript "hs_start"
  main :: IO ()



--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

data DOM  :: Effect
type instance DispatchOf DOM  = Static WithSideEffects
newtype instance StaticRep DOM = MkDOM ()

--
evalDOM :: IOE :> es => Eff (DOM : es) a -> Eff es a
evalDOM = evalStaticRep (MkDOM ())

data JSIO :: Effect
type instance DispatchOf JSIO = Static WithSideEffects
newtype instance StaticRep JSIO = MkJSIO ()

evalJSIO :: IOE :> es => Eff (JSIO : es) a -> Eff es a
evalJSIO = evalStaticRep (MkJSIO ())


-- type ES msg = [DOM,JSIO,IOE]

-- evalInIO :: Eff ES a -> IO a
-- evalInIO = runEff . evalJSIO . evalDOM

--------------------------------------------------------------------------------

jsDocument :: DOM :> es => Eff es Document
jsDocument = unsafeEff_ js_document

jsBody :: DOM :> es => Eff es Body
jsBody = unsafeEff_ js_body

jsWindow :: DOM :> es => Eff es Window
jsWindow = unsafeEff_ js_window

getParent :: (DOM :> es, IsNode element) => element -> Eff es Element
getParent = unsafeEff_ . coerce . js_getParent . asNode
  -- FIXME; the coerce here is hacky

--------------------------------------------------------------------------------

createTextNode :: DOM :> es => Text -> Eff es Node
createTextNode = unsafeEff_ . js_createTextNode . textToJSString

createElement                        :: DOM :> es => ElementName -> Eff es Element
createElement (ElementName elemName) = unsafeEff_ $ js_createElement (textToJSString elemName)


appendChild              :: (DOM :> es, IsNode parent, IsNode child)
                         => parent -> child  -> Eff es ()
appendChild parent child = unsafeEff_  $ js_appendChild (asNode parent) (asNode child)


removeChild              :: (DOM :> es, IsNode parent, IsNode child)
                         => parent -> child  -> Eff es ()
removeChild parent child = unsafeEff_  $ js_removeChild (asNode parent) (asNode child)


--------------------------------------------------------------------------------

-- | pre: element is of type 'el'
setAttribute               :: forall el a es element.
                              (IsNode element, DOM :> es, HasSetAttributeValue a)
                           => element -> HtmlAttribute el a -> a -> Eff es ()
setAttribute el attr value = unsafeEff_ $
    js_setAttribute (asNode el) (textToJSString $ attrNameOf attr) value

-- | Removes an attribute
removeAttribute         :: (IsNode element, DOM :> es)
                        => element -> HtmlAttribute el a -> Eff es ()
removeAttribute el attr = unsafeEff_ $
    js_removeAttribute (asNode el) (textToJSString $ attrNameOf attr)

--------------------------------------------------------------------------------

-- type CanSchedule handlerEs = Reader.Reader (Eff handlerEs () -> Eff '[JSIO] ())


-- data CanSchedule handlerEs :: Effect where
--   SetupEventHandler :: CanSchedule handlerEs m (Eff handlerEs () -> IO ())

-- type instance DispatchOf (CanSchedule handlerEs) = Dynamic

-- setupEventHandler :: ( CanSchedule handlerEs :> es
--                      , HasCallStack
--                      )
--                   => Eff es (Eff handlerEs () -> Eff '[JSIO] ())
-- setupEventHandler = Reader.ask


-- withEventSetup :: ( Eff handlerEs () -> IO () )
--                -> Eff handlerEs a
--                -> Eff es a
-- withEventSetup

-- schedule ::

--   Eff (CanSchedule handlerEs : es) a -> Eff es a
-- schedule = Reader.runReader



-- setupEventHandler = send



-- data CanSchedule handlerEs es where
--   schedule ::

consoleLog :: JSIO :> es => Text -> Eff es ()
consoleLog = unsafeEff_  . js_log . textToJSString

-- | Type that explains how to actually run an EventHandler in IO
type EventHandlerRunner handlerEs = Eff handlerEs () -> IO ()

-- | Shorthand
type CanRunHandler handlerEs = Reader (EventHandlerRunner handlerEs)

-- | Add an Event Listener.
addEventListener                           :: forall handlerEs es eventTarget.
                                              ( IsEventTarget eventTarget
                                              , DOM                     :> es
                                              , CanRunHandler handlerEs :> es
                                              )
                                           => eventTarget
                                           -> EventAttr
                                           -> (Event -> Eff handlerEs ())
                                           -> Eff es ()
addEventListener target eventType listener = do
    -- get the eventHandlerRunner; i.e. the thing that we use to run the Eff hanlder () in the
    -- IO monad.
    runListener <- ask @(Eff handlerEs () -> IO ())
    let jsListener :: JSVal -> IO ()
        jsListener = runListener . listener . Event
    unsafeEff_  $ do
      -- we create the callback
      listenerRef   <- js_mkEventHandler jsListener
      js_addEventListener (asEventTarget target)
                          (textToJSString . coerce $ asEventType eventType)
                          listenerRef

removeEventListener                 :: (IsEventTarget eventTarget, DOM :> es)
                                    => eventTarget
                                    -> EventType
                                    -> JSVal -- EventListener ()
                                    -> Eff es ()
removeEventListener target
                    (EventType eventType)
                    listener' = unsafeEff_ $
  js_remove_event_listener (asEventTarget target) (textToJSString eventType) listener'


-- onLoad     :: DOM :> es => Eff ES () -> Eff es ()
-- onLoad act = do window <- jsWindow
--                 addEventListener window OnLoad
--                                  (EventListener . const $ evalInIO act)

-- onClick    :: DOM :> es => Eff ES () -> Eff es ()
-- onClick act = do document <- jsDocument
--                  addEventListener document OnClick (EventListener . const $ evalInIO act)

--------------------------------------------------------------------------------


update :: model -> msg -> Eff es model
update = undefined


--------------------------------------------------------------------------------

registerEventHandles                :: ( IsEventTarget target
                                       , DOM                     :> es
                                       , CanRunHandler handlerEs :> es
                                       )
                                    => ( msg -> Event -> Eff handlerEs () )
                                         -- ^ our event handler
                                    -> target
                                    -> Map EventAttr msg -> Eff es ()
registerEventHandles handler target = Map.foldMapWithKey $ \evt msg ->
                                        addEventListener (asEventTarget target)
                                                         evt
                                                         (handler msg)



--------------------------------------------------------------------------------

data Send msg :: Effect where
  SendMessage :: msg -> Send msg m ()

type instance DispatchOf (Send msg) = Dynamic

sendMessage :: (Send msg :> es, HasCallStack) => msg -> Eff es ()
sendMessage = Eff.send . SendMessage

-- | A way of implementing send
runSendWith       :: Concurrent :> es
                  => TBQueue msg -> Eff (Send msg : es) a -> Eff es a
runSendWith queue = interpret $ \_ -> \case
    SendMessage msg -> atomically $ writeTBQueue queue msg
-- I want this to be pretty mcuh the only way of implemething send?

-- evalSend


-- data Send msg :: Effect

-- type instance DispatchOf (Send msg)  = Static WithSideEffects
-- newtype instance StaticRep (Send msg) = SendDispatch (TBQueue msg)

-- sendMessage     :: (Send msg :> es, Concurrent es) => msg -> Eff es ()
-- sendMessage msg = do SendDispatch queue <- getStaticRep
--                      writeTBQueue es


--------------------------------------------------------------------------------

type View msg = Html () msg

data App handlerEs msg model =
  App { appInitialModel  :: model
      -- , appUpdate        :: model -> msg -> Eff es model
      , appUpdate        :: model -> msg -> Eff handlerEs model
      , appRenderView    :: model -> View msg
      , appInitialAction :: Maybe msg
      }

runApp     :: forall handlerEs es msg model.
              ( Concurrent :> es
              -- , Send msg   :> handlerEs

              , DOM        :> es
              , JSIO       :> es
              , Concurrent :> es

              , IOE        :> es

              -- , IOE        :> handlerEs
                -- handlerEs ~ [Send msg, DOM, Concurrent, JSIO]
              , handlerEs ~ [Send msg, DOM, JSIO, Concurrent, IOE]
              )
           => App handlerEs msg model -> Eff es ()
runApp app = do
               queue <- atomically $ do q <- newTBQueue queueSize
                                        for_ (appInitialAction app) $ writeTBQueue q
                                        pure q

               startApp queue (appInitialModel app)
  where
    startApp       :: TBQueue msg -> model -> Eff es ()
    startApp queue = void . handle
      where
        runner :: EventHandlerRunner handlerEs
        runner = runEff
               . runConcurrent
               . evalJSIO
               . evalDOM
               . runSendWith queue

        -- runner' :: Eff handlerEs a -> Eff es a
        -- runner' = runConcurrent
        --         . evalJSIO
        --         . evalDOM
        --         . runSendWith queue

        runInEff :: Eff handlerEs a -> Eff es a
        runInEff = inject . runSendWith queue

        handle       :: model -> Eff es model
        handle model = do msg    <- atomically $ readTBQueue queue
                          model' <- runInEff $ (appUpdate app) model msg
                                    -- this doesn't seem right yet.
                          handle model'



-- runSendWith queue
--                    . Reader.runReader runner
--                    .




  -- do
  --              body   <- jsBody
  --              tr <- createHtml body (myUI myModel)


queueSize = 1000


--------------------------------------------------------------------------------

data MyModel = MyModel Text
  deriving (Show,Eq)

myModel :: MyModel
myModel = MyModel "initial model"

data MyMsg = HasBeenClicked
           | SetMsg Text
           | MyInitialAction

myApp :: ( JSIO :> es
         , DOM  :> es -- FIXME
         ) => App es MyMsg MyModel
myApp = App { appInitialModel  = myModel
            , appInitialAction = Just MyInitialAction
            , appUpdate        = myUpdate
            , appRenderView    = myUI
            }


myUpdate   :: ( JSIO :> es
              , DOM  :> es -- not sure if I want this
              -- , CanSchedule handlerEs :> es
              ) => MyModel -> MyMsg -> Eff es MyModel
myUpdate m = \case
    HasBeenClicked  -> do consoleLog "hasbeen clicked :)"
                          pure m
    SetMsg t        -> do consoleLog "setting msg"
                          pure $ MyModel t
    MyInitialAction -> do consoleLog "initial Action"
                          body   <- jsBody
                          -- _tr <- createHtml body (myUI myModel)
                          pure m

--------------------------------------------------------------------------------

-- newtype AttributeValue = AttributeValue Text
--   deriving stock (Show,Eq,Ord)
--   deriving newtype (IsString)


-- newtype AttrKey = AttrKey Text
--   deriving stock (Show,Eq,Ord)
--   deriving newtype (IsString)

-- type Attributes = DMap AttributeValue

-- data HtmlIx = TextIx | NodeIx [HtmlIx]


data Html a msg where
  TextNode :: !Text           -> a -> Html a msg
  HtmlNode :: !HtmlElement
           -> a
           -> Map EventAttr msg
           -> Attributes el -- this should match the htmlElemnt as well...
           -> Seq (Html a msg)
                              -> Html a msg

deriving instance Functor     (Html a)
deriving instance Foldable    (Html a)
deriving instance Traversable (Html a)

instance Bifunctor Html where
  bimap f g = go
    where
      go = \case
        TextNode text x              -> TextNode text (f x)
        HtmlNode el x evts attrs chs -> HtmlNode el   (f x) (g <$> evts) attrs (go <$> chs)

instance Bifoldable Html where
  bifoldMap f g = go
    where
      go = \case
        TextNode text x              -> f x
        HtmlNode el x evts attrs chs -> f x <> foldMap g evts <> foldMap go chs

instance Bitraversable Html where
  bitraverse f g = go
    where
      go = \case
        TextNode text x              -> TextNode text <$> f x
        HtmlNode el x evts attrs chs -> (\x' evts' chs' -> HtmlNode el x' evts' attrs chs')
                                    <$> f x
                                    <*> traverse g evts
                                    <*> traverse go chs




           -- deriving (Show,Eq)



-- TODO: we should give this the appending function somehow

createHtml        :: forall handlerEs es root a msg.
                     ( IsNode root
                     , DOM                     :> es
                     , CanRunHandler handlerEs :> es
                     , Send msg                :> handlerEs
                     , JSIO                    :> handlerEs
                     )
                  => root -> Html a msg -> Eff es (Html Element msg)
createHtml parent = \case
    TextNode text _              -> do txtRef <- createTextNode text
                                       appendChild parent txtRef
                                       pure $ TextNode text (coerce txtRef) -- TODO
    HtmlNode el _ evts attrs chs -> do elRef <- createElement (elementNameOf el)
                                       appendChild parent elRef
                                       -- set attrs
                                       -- setAttribute elRef Id "foo"
                                       traverseAttributes_ (\attr value ->
                                         has @HasSetAttributeValue attr
                                            setAttribute elRef attr value) attrs

                                       -- register event handles
                                       registerEventHandles handler elRef evts

                                       chs' <- traverse (createHtml @handlerEs elRef) chs
                                       pure $ HtmlNode el elRef evts attrs chs'
  where
    handler         :: msg -> Event -> Eff handlerEs ()
    handler msg evt = do consoleLog "should parse the evt"
                         sendMessage msg

-- schedule     :: msg -> Eff ES ()
-- schedule msg = consoleLog "schedule"

  -- do m' <- myUpdate (MyModel "dummy") msg
  --                 consoleLog $ "result from update" <> showT m'



{-
class HasPatch a where
  -- | The type represetning a patch to the given data.
  data family Patch a :: Type

  -- | returns some description of the operation we have to do to genearte the diff
  diff :: a
       -- ^ orig
       -> a
       -- ^ new
       -> Maybe (Patch a)

  -- | Apply the patch, possibly doing some side effect
  patch :: a -> Patch a -> Eff es a



-- | We use a very blunt approach for patching text

instance HasPatch Text where
  newtype Patch Text = ReplaceBy Text
    deriving stock (Show,Eq,Ord)

  diff orig new
    | orig == new = Nothing
    | otherwise  = Just new

  patch _ (ReplaceBy t) = pure t

instance HasPatch (Html a msg) where
  data Patch (Html a msg) = UpdateTextNode TextNode text

  -- the message part is kind of interesting ...
  -- since the html doesn't really change I guess

-}


-- |
patchHtml          :: forall es handlerEs msg a.
                      ( DOM :> es
                      , CanRunHandler handlerEs :> es
                      , Send msg :> handlerEs
                      , JSIO :> handlerEs
                      )
                   => Html Element msg -- ^ orig
                   -> Html a msg -- ^ new
                   -> Eff es (Maybe (Html Element msg))
patchHtml orig new =  case orig of
  TextNode oldText elRef            -> case new of
    TextNode newText _
      | oldText == newText -> pure Nothing
      | otherwise          -> undefined -- set text to newText
    _                      -> do parent <- getParent elRef
                                 trRef <- createHtml @handlerEs parent new -- TODO; add to the right place
                                 removeChild parent elRef
                                 pure undefined -- new with the data replaced

  HtmlNode el elRef evts attrs chs -> case new of
    TextNode text _                  -> do parent <- getParent elRef
                                           newRef <- createTextNode text
                                           -- TODO: add the text node
                                           pure $ Just (TextNode text (coerce newRef))
    HtmlNode el' _ evts' attrs' chs' -> do pure Nothing --old -- TODO








showT :: Show a => a -> Text
showT = Text.pack . show


-- maybe we should actually annotate the entire tree instead ..

textNode   :: Text -> Html () msg
textNode t = TextNode t mempty

-- | Helper data type moddeling assignments to events or attributes.
data Attr (el :: HtmlElement) (msg :: Type) =           !EventAttr            :- msg
                                            | forall a. !(HtmlAttribute el a) := a

infixr 1 :=, :-


htmlElement            :: forall el msg. ()
                       => HtmlElement
                       -> [Attr el msg]
                       -> [Html () msg]
                       -> Html () msg
htmlElement el ats chs = HtmlNode el mempty
                                     (Map.fromList  [(k,v) | k :- v <- ats])
                                     (attrsFromList [k DSum.:=> Identity v | k := v <- ats])
                                     (Seq.fromList chs)

div :: [Attr Div msg] -> [Html () msg] -> Html () msg
div = htmlElement @Div Div

p :: [Attr P msg] -> [Html () msg] -> Html () msg
p = htmlElement @P P

h1 :: [Attr H1 msg] -> [Html () msg] -> Html () msg
h1 = htmlElement @H1 H1


-- | Renders classes
classes :: Foldable f => f CssClass -> CssClass
classes = CssClass . Text.unwords . map coerce . F.toList

--------------------------------------------------------------------------------

myUI   :: MyModel -> Html () MyMsg
myUI m = div []
             [ h1  [ Class   := classes ["header", "someclass"]
                   , OnClick :- HasBeenClicked
                   , Id      := "theHeader"
                   ]
                   [ textNode "header!"
                   ]
             , div [] [p [ OnClick     :- SetMsg "woei"
                         , XData "foo" := "bar"
                         ]
                         [textNode "woei"]
                      ]
             ]





--------------------------------------------------------------------------------

main :: IO ()
main = runEff . runConcurrent . evalJSIO . evalDOM
     $ runApp myApp


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
