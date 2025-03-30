{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module EffWeb.Html.Type
  ( Html(..)

  , createHtml
  ) where

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
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           EffWeb.DOM.Effect
import           EffWeb.DOM.FFI
import           EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))
import           EffWeb.DOM.FFI.Types
import           EffWeb.Html.Attribute
import qualified EffWeb.Html.Attribute as A
import           EffWeb.Html.Element
import           EffWeb.Html.Event
import           EffWeb.JSIO
import           EffWeb.Send
import           Effectful


--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Creating Html in the DOM

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
