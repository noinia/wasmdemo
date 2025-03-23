{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Coerce
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Wasm.Prim

--------------------------------------------------------------------------------

type JSFunction = JSVal

foreign export javascript "hs_start"
  main :: IO ()


--------------------------------------------------------------------------------

foreign import javascript unsafe "return document"
  js_document :: IO Document

foreign import javascript unsafe "return document.body"
  js_body :: IO Body

foreign import javascript unsafe "return window"
  js_window :: IO Window

--------------------------------------------------------------------------------

foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()

--------------------------------------------------------------------------------

foreign import javascript unsafe "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO Node

foreign import javascript unsafe "document.createElement($1)"
  js_createElement :: JSString -> IO Element





foreign import javascript unsafe "$1.appendChild($2)"
  js_appendChild :: Node -> Node -> IO ()

foreign import javascript unsafe "$1.insertBefore($2)"
  js_insertBefore :: Node -> Node -> IO ()

foreign import javascript unsafe "$1.removeChild($2)"
  js_removeChild :: Node -> Node -> IO ()

--------------------------------------------------------------------------------

foreign import javascript "wrapper sync"
  js_mkEventHandler :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1.addEventListener($2,$3)"
  js_addEventListener :: EventTarget
                      -> JSString
                      -> JSVal
                      -> IO ()

foreign import javascript unsafe "$1.removeEventListener($2,$3)"
  js_remove_event_listener :: EventTarget -> JSString -> JSVal -> IO ()


--------------------------------------------------------------------------------

textToJSString :: Text -> JSString
textToJSString = toJSString . Text.unpack

--------------------------------------------------------------------------------

newtype ElementName = ElementName Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)

newtype EventType = EventType Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)

newtype EventTarget = EventTarget JSVal
--   deriving stock (Show,Eq,Ord)
--   deriving newtype (IsString)

class IsEventTarget target where
  asEventTarget :: target -> EventTarget


instance IsEventTarget EventTarget where
  asEventTarget = id

instance IsEventTarget Window where
  asEventTarget = coerce

instance IsEventTarget Document where
  asEventTarget = coerce


--------------------------------------------------------------------------------

newtype Element = Element JSVal

newtype Node = Node JSVal

newtype Event = Event JSVal
  -- deriving stock (Show,Eq,Ord)

newtype EventListener a = EventListener (Event -> IO a)

newtype Document = Document JSVal

newtype Window = Window JSVal

newtype Body = Body JSVal

class IsNode node where
  asNode :: node -> Node

instance IsNode Node where
  asNode = id

instance IsNode Body where
  asNode = coerce

instance IsNode Element where
  asNode = coerce

--------------------------------------------------------------------------------

jsDocument :: IO Document
jsDocument = js_document

jsBody :: IO Body
jsBody = js_body

jsWindow :: IO Window
jsWindow = js_window

createTextNode :: Text -> IO Node
createTextNode = js_createTextNode . textToJSString

createElement                        :: ElementName -> IO Element
createElement (ElementName elemName) = js_createElement (textToJSString elemName)


appendChild              :: (IsNode parent, IsNode child)
                         => parent -> child  -> IO ()
appendChild parent child = js_appendChild (asNode parent) (asNode child)


removeChild              :: (IsNode parent, IsNode child)
                         => parent -> child  -> IO ()
removeChild parent child = js_removeChild (asNode parent) (asNode child)


--------------------------------------------------------------------------------

consoleLog :: Text -> IO ()
consoleLog = js_log . textToJSString

addEventListener                    :: IsEventTarget eventTarget
                                    => eventTarget
                                    -> EventType
                                    -> EventListener ()
                                    -> IO ()
addEventListener target
                 (EventType eventType)
                 (EventListener listener) = do
  listener' <- js_mkEventHandler (coerce @_ @(JSVal -> IO ()) listener)
  js_addEventListener (asEventTarget target) (textToJSString eventType) listener'

removeEventListener                 :: IsEventTarget eventTarget
                                    => eventTarget
                                    -> EventType
                                    -> JSVal -- EventListener ()
                                    -> IO ()
removeEventListener target
                    (EventType eventType)
                    listener' =
  js_remove_event_listener (asEventTarget target) (textToJSString eventType) listener'


onLoad     :: IO () -> IO ()
onLoad act = do window <- jsWindow
                addEventListener window "load" (EventListener $ const act)

onClick act = do document <- jsDocument
                 addEventListener document "click" (EventListener $ const act)

main :: IO ()
main = do consoleLog "woei"
          onLoad $ do
            consoleLog "loaded"
            body   <- jsBody
            textNode <- createTextNode "my text on load"
            appendChild body textNode

          onClick $ do
            consoleLog "clicked"
            body   <- jsBody
            theDiv <- createElement "div"

            appendChild body theDiv

            textNode <- createTextNode "my text node :) "
            appendChild body textNode

          consoleLog "added"
