{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

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
import           EffWeb.DOM.Effect
import           EffWeb.DOM.FFI
import           EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))
import           EffWeb.DOM.FFI.Types
import           EffWeb.Html
import           EffWeb.Html.Type
import           EffWeb.Html.Attribute
import qualified EffWeb.Html.Attribute as A
import           EffWeb.Html.Element
import           EffWeb.Html.Event
import           EffWeb.JSIO
import           EffWeb.Send
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


--------------------------------------------------------------------------------




-- type ES msg = [DOM,JSIO,IOE]

-- evalInIO :: Eff ES a -> IO a
-- evalInIO = runEff . evalJSIO . evalDOM

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


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


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

type View msg = Html () msg

data App handlerEs msg model =
  App { appInitialModel  :: model
      -- , appUpdate        :: model -> msg -> Eff es model
      , appUpdate        :: model -> msg -> Eff handlerEs model
      , appRenderView    :: model -> View msg
      , appInitialAction :: Maybe msg
      }


type SyncES msg = [Send msg, JSIO, Concurrent, IOE]

runApp     :: forall handlerEs es msg model.
              ( Concurrent :> es
              , DOM        :> es
              , JSIO       :> es

              , IOE        :> es  -- we should avoid this one ...
              , Subset handlerEs es



              -- , JSIO       :> es
              -- , Concurrent :> handlerEs

              -- , IOE        :> es

              -- , IOE        :> handlerEs
                -- handlerEs ~ [Send msg, DOM, Concurrent, JSIO]
              -- , handlerEs ~ [Send msg, DOM, JSIO, Concurrent, IOE]
              -- , es
              )
           => App handlerEs msg model -> Eff es ()
runApp app@(App { appUpdate        = updateHandler
                , appRenderView    = renderView
                }
           ) = do
                 queue <- atomically $ do q <- newTBQueue queueSize
                                          for_ (appInitialAction app) $ writeTBQueue q
                                          pure q

                 body     <- jsBody
                 startApp queue body

  where
    startApp            :: TBQueue msg -> Body -> Eff es ()
    startApp queue body = do
                            htmlTree <- runReader runner $
                              createHtml @(SyncES msg) body $ renderView (appInitialModel app)
                            handle htmlTree (appInitialModel app)
      where
        handle                :: Html Element msg -> model -> Eff es ()
        handle htmlTree model = do msg    <- atomically $ readTBQueue queue
                                   model' <- runInEff $ updateHandler model msg
                                   handle htmlTree model'
                                   -- we may wish to diff the tree

        runInEff :: Eff handlerEs a -> Eff es a
        runInEff = inject
          -- . runSendWith queue

        runner :: EventHandlerRunner (SyncES msg)
        runner = runEff
               . runConcurrent
               . evalJSIO
               -- . evalDOM
               . runSendWith queue


-- runSendWith queue
--                    . Reader.runReader runner
--                    .




  -- do
  --              body   <- jsBody
  --              tr <- createHtml body (myUI myModel)


queueSize = 1000


--------------------------------------------------------------------------------

data MyModel = MyModel { modelText :: Text }
  deriving (Show,Eq)

myModel :: MyModel
myModel = MyModel "initial model"

data MyMsg = HasBeenClicked
           | SetMsg Text
           | MyInitialAction

myApp :: ( JSIO :> es
         ) => App es MyMsg MyModel
myApp = App { appInitialModel  = myModel
            , appInitialAction = Just MyInitialAction
            , appUpdate        = myUpdate
            , appRenderView    = myUI
            }


myUpdate   :: ( JSIO :> es
              ) => MyModel -> MyMsg -> Eff es MyModel
myUpdate m = \case
    HasBeenClicked  -> do consoleLog "hasbeen clicked :)"
                          pure m
    SetMsg t        -> do consoleLog "setting msg"
                          pure $ MyModel t
    MyInitialAction -> do consoleLog "initial Action"
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


-- schedule     :: msg -> Eff ES ()
-- schedule msg = consoleLog "schedule"

  -- do m' <- myUpdate (MyModel "dummy") msg
  --                 consoleLog $ "result from update" <> showT m'




showT :: Show a => a -> Text
showT = Text.pack . show


-- maybe we should actually annotate the entire tree instead ..

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
                         , A.Style     := "border: 1px solid black; width: 200px; height: 100px;"
                         , OnMouseOver :- SetMsg "hovering"
                         ]
                         [ textNode $ modelText m
                         ]
                      ]
             ]





--------------------------------------------------------------------------------

main :: IO ()
main = runEff . runConcurrent . evalJSIO . evalDOM -- $ runApp myApp
     $ main'
  where
    main' :: Eff [DOM, JSIO, Concurrent, IOE] ()
    main' = runApp @[JSIO, IOE] myApp


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
