module EffWeb.DOM.FFI where

import           Data.Coerce
import           Data.Text (Text)
import qualified Data.Text as Text
import           EffWeb.DOM.Effect
import           EffWeb.DOM.FFI.Raw
import           EffWeb.DOM.FFI.Types
import           EffWeb.Html.Attribute
import           EffWeb.Html.Element
import           EffWeb.Html.Event
import           EffWeb.JSIO
import           Effectful
import           Effectful.Dispatch.Static
import           Effectful.Reader.Static
import           GHC.Wasm.Prim


--------------------------------------------------------------------------------
-- * Global elements

jsDocument :: DOM :> es => Eff es Document
jsDocument = unsafeEff_ js_document

jsBody :: DOM :> es => Eff es Body
jsBody = unsafeEff_ js_body

jsWindow :: DOM :> es => Eff es Window
jsWindow = unsafeEff_ js_window


--------------------------------------------------------------------------------
-- * Querying things

getParent :: (DOM :> es, IsNode element) => element -> Eff es Element
getParent = unsafeEff_ . coerce . js_getParent . asNode
  -- FIXME; the coerce here is hacky


--------------------------------------------------------------------------------
-- * Text

createTextNode :: DOM :> es => Text -> Eff es Node
createTextNode = unsafeEff_ . js_createTextNode . textToJSString

--------------------------------------------------------------------------------
-- * Elements

createElement                        :: DOM :> es => ElementName -> Eff es Element
createElement (ElementName elemName) = unsafeEff_ $ js_createElement (textToJSString elemName)


appendChild              :: (DOM :> es, IsNode parent, IsNode child)
                         => parent -> child  -> Eff es ()
appendChild parent child = unsafeEff_  $ js_appendChild (asNode parent) (asNode child)


removeChild              :: (DOM :> es, IsNode parent, IsNode child)
                         => parent -> child  -> Eff es ()
removeChild parent child = unsafeEff_  $ js_removeChild (asNode parent) (asNode child)

--------------------------------------------------------------------------------
-- * Attributes

-- | pre: element is of type 'el'
setAttribute               :: forall el msg a es element.
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
-- Console

consoleLog :: JSIO :> es => Text -> Eff es ()
consoleLog = unsafeEff_  . js_log . textToJSString


--------------------------------------------------------------------------------
-- * Events

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
