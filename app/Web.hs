{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Attributes
import           Data.Coerce
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           Effectful
import           Effectful.Dispatch.Static
import           GHC.Wasm.Prim
import           HtmlElement
import           HtmlEvent
import           Prelude hiding (div)


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


type ES = [DOM,JSIO,IOE]
evalInIO :: Eff ES a -> IO a
evalInIO = runEff . evalJSIO . evalDOM

--------------------------------------------------------------------------------

jsDocument :: DOM :> es => Eff es Document
jsDocument = unsafeEff_ js_document

jsBody :: DOM :> es => Eff es Body
jsBody = unsafeEff_ js_body

jsWindow :: DOM :> es => Eff es Window
jsWindow = unsafeEff_ js_window

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

consoleLog :: JSIO :> es => Text -> Eff es ()
consoleLog = unsafeEff_  . js_log . textToJSString

addEventListener                    :: (IsEventTarget eventTarget, DOM :> es)
                                    => eventTarget
                                    -> EventType
                                    -> EventListener ()
                                    -> Eff es ()
addEventListener target
                 (EventType eventType)
                 (EventListener listener) = unsafeEff_  $ do
  listener' <- js_mkEventHandler (coerce @_ @(JSVal -> IO ()) listener)
  js_addEventListener (asEventTarget target) (textToJSString eventType) listener'

removeEventListener                 :: (IsEventTarget eventTarget, DOM :> es)
                                    => eventTarget
                                    -> EventType
                                    -> JSVal -- EventListener ()
                                    -> Eff es ()
removeEventListener target
                    (EventType eventType)
                    listener' = unsafeEff_ $
  js_remove_event_listener (asEventTarget target) (textToJSString eventType) listener'


onLoad     :: DOM :> es => Eff ES () -> Eff es ()
onLoad act = do window <- jsWindow
                addEventListener window "load"
                                 (EventListener . const $ evalInIO act)

onClick    :: DOM :> es => Eff ES () -> Eff es ()
onClick act = do document <- jsDocument
                 addEventListener document "click" (EventListener . const $ evalInIO act)

--------------------------------------------------------------------------------

newtype AttributeName = AttributeName Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)

-- newtype AttributeValue = AttributeValue Text
--   deriving stock (Show,Eq,Ord)
--   deriving newtype (IsString)


-- newtype AttrKey = AttrKey Text
--   deriving stock (Show,Eq,Ord)
--   deriving newtype (IsString)

-- type Attributes = DMap AttributeValue

-- data HtmlIx = TextIx | NodeIx [HtmlIx]


data Html msg where
  TextNode :: !Text           -> Html msg
  HtmlNode :: !HtmlElement
           -> Map EventAttr msg
           -> Attributes el -- this should match the htmlElemnt as well...
           -> Seq (Html msg)
                              -> Html msg

           -- deriving (Show,Eq)

createHtml        :: (DOM :> es, IsNode root)
                  => root -> Html msg -> Eff es Element
createHtml parent = \case
  TextNode text              -> do txtRef <- createTextNode text
                                   appendChild parent txtRef
                                   pure $ coerce txtRef -- TODO
  HtmlNode el evts attrs chs -> do elRef <- createElement (elementNameOf el)
                                   appendChild parent elRef
                                   -- set attrs
                                   traverse_ (createHtml elRef) chs
                                   pure elRef

-- maybe we should actually annotate the entire tree instead ..

textNode :: Text -> Html msg
textNode = TextNode

-- | Helper to construct event handles
data evt :-> msg = evt :-> msg deriving (Show,Eq)



htmlElement                   :: forall el msg.
                                 HtmlElement
                              -> [EventAttr :-> msg]
                              -> [HtmlAttr el]
                              -> [Html msg]
                              -> Html msg
htmlElement el evts attrs chs = HtmlNode el (Map.fromList [(k,v) | k :-> v <- evts])
                                            (attrsFromList attrs)
                                            (Seq.fromList chs)


div :: [EventAttr :-> msg] -> [HtmlAttr Div] -> [Html msg] -> Html msg
div = htmlElement @Div Div

p :: [EventAttr :-> msg] -> [HtmlAttr P] -> [Html msg] -> Html msg
p = htmlElement @P P

h1 :: [EventAttr :-> msg] -> [HtmlAttr H1] -> [Html msg] -> Html msg
h1 = htmlElement @H1 H1

--------------------------------------------------------------------------------

myUI :: Html msg
myUI = div []
           []
           [ h1  [] [] [textNode "header!"]
           , div [] [] [p [] [] [textNode "woei"]]
           ]


--------------------------------------------------------------------------------

main :: IO ()
main = runEff . evalJSIO . evalDOM
     $ do consoleLog "woei"
          body   <- jsBody

          tr <- createHtml body myUI

          textNode <- createTextNode "my text on load"
          appendChild body textNode
          -- onLoad $ do
          --   consoleLog "loaded"
          --   body   <- jsBody
          --   textNode <- createTextNode "my text on load"
          --   appendChild body textNode

          onClick $ do
            consoleLog "clicked"
            body   <- jsBody
            theDiv <- createElement "div"

            appendChild body theDiv

            textNode <- createTextNode "my text node :) "
            appendChild body textNode

          consoleLog "added"
