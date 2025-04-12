{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module EffWeb.Html.Event
  ( EventAttr(..)
  , asEventType

  , mapEvent
  , CanHandleEvent(..)

  , MouseEvent(..)
  , MousePosition(..)
  ) where

import           Data.Coerce
import           Data.Constraint.Extras
import           Data.Constraint.Extras.TH (deriveArgDict)
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum (DSum(..), (==>))
import           Data.Functor.Identity (Identity(..))
import           Data.GADT.Compare
import           Data.GADT.Compare.TH (deriveGEq,deriveGCompare)
import           Data.GADT.Show
import           Data.GADT.Show.TH (deriveGShow)
import           Data.Kind (Type)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))
import           EffWeb.DOM.FFI.Types
import           EffWeb.Html.Attribute.Common
import           EffWeb.Html.Element (HtmlElement)
import           EffWeb.Send
import           Effectful
import           GHC.TypeLits

--------------------------------------------------------------------------------

newtype MouseEvent = MouseEvent MousePosition
  deriving (Show,Eq)

data MousePosition = MousePosition Int Int
  deriving (Show,Eq) -- temporary

-- | Descriptions of possible events
data EventAttr msg a where
  OnAbort             :: EventAttr msg msg
  OnAutoComplete      :: EventAttr msg msg
  OnAutoCompleteError :: EventAttr msg msg
  OnBlur              :: EventAttr msg msg
  OnCancel            :: EventAttr msg msg
  OnCanplay           :: EventAttr msg msg
  OnCanplayThrough    :: EventAttr msg msg
  OnChange            :: EventAttr msg msg
  OnClick             :: EventAttr msg msg
  OnClose             :: EventAttr msg msg
  OnContextMenu       :: EventAttr msg msg
  OnCueChange         :: EventAttr msg msg
  OnDblClick          :: EventAttr msg msg
  OnDrag              :: EventAttr msg msg
  OnDragEnd           :: EventAttr msg msg
  OnDragEnter         :: EventAttr msg msg
  OnDragLeave         :: EventAttr msg msg
  OnDragOver          :: EventAttr msg msg
  OnDragStart         :: EventAttr msg msg
  OnDrop              :: EventAttr msg msg
  OnDurationChange    :: EventAttr msg msg
  OnEmptied           :: EventAttr msg msg
  OnEnded             :: EventAttr msg msg
  OnError             :: EventAttr msg msg
  OnFocus             :: EventAttr msg msg
  OnInput             :: EventAttr msg msg
  OnInvalid           :: EventAttr msg msg
  OnKeyDown           :: EventAttr msg msg
  OnKeyPress          :: EventAttr msg msg
  OnKeyUp             :: EventAttr msg msg
  OnLoad              :: EventAttr msg msg
  OnLoadedData        :: EventAttr msg msg
  OnLoadedMetaData    :: EventAttr msg msg
  OnLoadStart         :: EventAttr msg msg
  OnMouseDown         :: EventAttr msg (MouseEvent -> msg)
  OnMouseEnter        :: EventAttr msg (MouseEvent -> msg)
  OnMouseLeave        :: EventAttr msg (MouseEvent -> msg)
  OnMouseMove         :: EventAttr msg (MouseEvent -> msg)
  OnMouseOut          :: EventAttr msg (MouseEvent -> msg)
  OnMouseOver         :: EventAttr msg (MouseEvent -> msg)
  OnMouseUp           :: EventAttr msg (MouseEvent -> msg)
  OnMouseWheel        :: EventAttr msg (MouseEvent -> msg)
  OnPause             :: EventAttr msg msg
  OnPlay              :: EventAttr msg msg
  OnPlaying           :: EventAttr msg msg
  OnProgress          :: EventAttr msg msg
  OnRateChange        :: EventAttr msg msg
  OnReset             :: EventAttr msg msg
  OnResize            :: EventAttr msg msg
  OnScroll            :: EventAttr msg msg
  OnSeeked            :: EventAttr msg msg
  OnSeeking           :: EventAttr msg msg
  OnSelect            :: EventAttr msg msg
  OnShow              :: EventAttr msg msg
  OnSort              :: EventAttr msg msg
  OnStalled           :: EventAttr msg msg
  OnSubmit            :: EventAttr msg msg
  OnSuspend           :: EventAttr msg msg
  OnTimeUpdate        :: EventAttr msg msg
  OnToggle            :: EventAttr msg msg
  OnVolumeChange      :: EventAttr msg msg
  OnWaiting           :: EventAttr msg msg

deriving instance (Show msg, Show a) => Show (EventAttr msg a)

-- | Change the event type
mapEvent                  :: Functor f
                          => (msg -> msg') -> DSum (EventAttr msg) f -> DSum (EventAttr msg') f
mapEvent f (evt :=> fval) = case evt of
    OnAbort             -> OnAbort               :=> fmap f fval
    OnAutoComplete      -> OnAutoComplete        :=> fmap f fval
    OnAutoCompleteError -> OnAutoCompleteError   :=> fmap f fval
    OnBlur              -> OnBlur                :=> fmap f fval
    OnCancel            -> OnCancel              :=> fmap f fval
    OnCanplay           -> OnCanplay             :=> fmap f fval
    OnCanplayThrough    -> OnCanplayThrough      :=> fmap f fval
    OnChange            -> OnChange              :=> fmap f fval
    OnClick             -> OnClick               :=> fmap f fval
    OnClose             -> OnClose               :=> fmap f fval
    OnContextMenu       -> OnContextMenu         :=> fmap f fval
    OnCueChange         -> OnCueChange           :=> fmap f fval
    OnDblClick          -> OnDblClick            :=> fmap f fval
    OnDrag              -> OnDrag                :=> fmap f fval
    OnDragEnd           -> OnDragEnd             :=> fmap f fval
    OnDragEnter         -> OnDragEnter           :=> fmap f fval
    OnDragLeave         -> OnDragLeave           :=> fmap f fval
    OnDragOver          -> OnDragOver            :=> fmap f fval
    OnDragStart         -> OnDragStart           :=> fmap f fval
    OnDrop              -> OnDrop                :=> fmap f fval
    OnDurationChange    -> OnDurationChange      :=> fmap f fval
    OnEmptied           -> OnEmptied             :=> fmap f fval
    OnEnded             -> OnEnded               :=> fmap f fval
    OnError             -> OnError               :=> fmap f fval
    OnFocus             -> OnFocus               :=> fmap f fval
    OnInput             -> OnInput               :=> fmap f fval
    OnInvalid           -> OnInvalid             :=> fmap f fval
    OnKeyDown           -> OnKeyDown             :=> fmap f fval
    OnKeyPress          -> OnKeyPress            :=> fmap f fval
    OnKeyUp             -> OnKeyUp               :=> fmap f fval
    OnLoad              -> OnLoad                :=> fmap f fval
    OnLoadedData        -> OnLoadedData          :=> fmap f fval
    OnLoadedMetaData    -> OnLoadedMetaData      :=> fmap f fval
    OnLoadStart         -> OnLoadStart           :=> fmap f fval
    OnMouseDown         -> OnMouseDown           :=> fmap (fmap f) fval
    OnMouseEnter        -> OnMouseEnter          :=> fmap (fmap f) fval
    OnMouseLeave        -> OnMouseLeave          :=> fmap (fmap f) fval
    OnMouseMove         -> OnMouseMove           :=> fmap (fmap f) fval
    OnMouseOut          -> OnMouseOut            :=> fmap (fmap f) fval
    OnMouseOver         -> OnMouseOver           :=> fmap (fmap f) fval
    OnMouseUp           -> OnMouseUp             :=> fmap (fmap f) fval
    OnMouseWheel        -> OnMouseWheel          :=> fmap (fmap f) fval
    OnPause             -> OnPause               :=> fmap f fval
    OnPlay              -> OnPlay                :=> fmap f fval
    OnPlaying           -> OnPlaying             :=> fmap f fval
    OnProgress          -> OnProgress            :=> fmap f fval
    OnRateChange        -> OnRateChange          :=> fmap f fval
    OnReset             -> OnReset               :=> fmap f fval
    OnResize            -> OnResize              :=> fmap f fval
    OnScroll            -> OnScroll              :=> fmap f fval
    OnSeeked            -> OnSeeked              :=> fmap f fval
    OnSeeking           -> OnSeeking             :=> fmap f fval
    OnSelect            -> OnSelect              :=> fmap f fval
    OnShow              -> OnShow                :=> fmap f fval
    OnSort              -> OnSort                :=> fmap f fval
    OnStalled           -> OnStalled             :=> fmap f fval
    OnSubmit            -> OnSubmit              :=> fmap f fval
    OnSuspend           -> OnSuspend             :=> fmap f fval
    OnTimeUpdate        -> OnTimeUpdate          :=> fmap f fval
    OnToggle            -> OnToggle              :=> fmap f fval
    OnVolumeChange      -> OnVolumeChange        :=> fmap f fval
    OnWaiting           -> OnWaiting             :=> fmap f fval

instance GEq   (EventAttr msg) where geq = defaultGeq
instance GCompare  (EventAttr msg) where
  gcompare _ _ = GGT -- FIXME !!

-- deriveGEq      ''EventAttr
-- deriveGCompare ''EventAttr

deriveGShow    ''EventAttr

instance ( -- forall a. c msg a
           c msg msg
         , c msg (MouseEvent -> msg)
         ) => Has (c msg) (EventAttr msg)  where
  has a x = case a of
    OnAbort             -> x
    OnAutoComplete      -> x
    OnAutoCompleteError -> x
    OnBlur              -> x
    OnCancel            -> x
    OnCanplay           -> x
    OnCanplayThrough    -> x
    OnChange            -> x
    OnClick             -> x
    OnClose             -> x
    OnContextMenu       -> x
    OnCueChange         -> x
    OnDblClick          -> x
    OnDrag              -> x
    OnDragEnd           -> x
    OnDragEnter         -> x
    OnDragLeave         -> x
    OnDragOver          -> x
    OnDragStart         -> x
    OnDrop              -> x
    OnDurationChange    -> x
    OnEmptied           -> x
    OnEnded             -> x
    OnError             -> x
    OnFocus             -> x
    OnInput             -> x
    OnInvalid           -> x
    OnKeyDown           -> x
    OnKeyPress          -> x
    OnKeyUp             -> x
    OnLoad              -> x
    OnLoadedData        -> x
    OnLoadedMetaData    -> x
    OnLoadStart         -> x
    OnMouseDown         -> x
    OnMouseEnter        -> x
    OnMouseLeave        -> x
    OnMouseMove         -> x
    OnMouseOut          -> x
    OnMouseOver         -> x
    OnMouseUp           -> x
    OnMouseWheel        -> x
    OnPause             -> x
    OnPlay              -> x
    OnPlaying           -> x
    OnProgress          -> x
    OnRateChange        -> x
    OnReset             -> x
    OnResize            -> x
    OnScroll            -> x
    OnSeeked            -> x
    OnSeeking           -> x
    OnSelect            -> x
    OnShow              -> x
    OnSort              -> x
    OnStalled           -> x
    OnSubmit            -> x
    OnSuspend           -> x
    OnTimeUpdate        -> x
    OnToggle            -> x
    OnVolumeChange      -> x
    OnWaiting           -> x


-- | Get the Name of a HtmlAttribute
instance HasAttrName (EventAttr msg a) where
  attrNameOf = coerce . asEventType

-- | Render as event type
asEventType :: EventAttr msg a -> EventType
asEventType = EventType . \case
  OnAbort             -> "abort"
  OnAutoComplete      -> "autocomplete"
  OnAutoCompleteError -> "autocompleteerror"
  OnBlur              -> "blur"
  OnCancel            -> "cancel"
  OnCanplay           -> "canplay"
  OnCanplayThrough    -> "canplaythrough"
  OnChange            -> "change"
  OnClick             -> "click"
  OnClose             -> "close"
  OnContextMenu       -> "contextmenu"
  OnCueChange         -> "cuechange"
  OnDblClick          -> "dblclick"
  OnDrag              -> "drag"
  OnDragEnd           -> "dragend"
  OnDragEnter         -> "dragenter"
  OnDragLeave         -> "dragleave"
  OnDragOver          -> "dragover"
  OnDragStart         -> "dragstart"
  OnDrop              -> "drop"
  OnDurationChange    -> "durationchange"
  OnEmptied           -> "emptied"
  OnEnded             -> "ended"
  OnError             -> "error"
  OnFocus             -> "focus"
  OnInput             -> "input"
  OnInvalid           -> "invalid"
  OnKeyDown           -> "keydown"
  OnKeyPress          -> "keypress"
  OnKeyUp             -> "keyup"
  OnLoad              -> "load"
  OnLoadedData        -> "loadeddata"
  OnLoadedMetaData    -> "loadedmetadata"
  OnLoadStart         -> "loadstart"
  OnMouseDown         -> "mousedown"
  OnMouseEnter        -> "mouseenter"
  OnMouseLeave        -> "mouseleave"
  OnMouseMove         -> "mousemove"
  OnMouseOut          -> "mouseout"
  OnMouseOver         -> "mouseover"
  OnMouseUp           -> "mouseup"
  OnMouseWheel        -> "mousewheel"
  OnPause             -> "pause"
  OnPlay              -> "play"
  OnPlaying           -> "playing"
  OnProgress          -> "progress"
  OnRateChange        -> "ratechange"
  OnReset             -> "reset"
  OnResize            -> "resize"
  OnScroll            -> "scroll"
  OnSeeked            -> "seeked"
  OnSeeking           -> "seeking"
  OnSelect            -> "select"
  OnShow              -> "show"
  OnSort              -> "sort"
  OnStalled           -> "stalled"
  OnSubmit            -> "submit"
  OnSuspend           -> "suspend"
  OnTimeUpdate        -> "timeupdate"
  OnToggle            -> "toggle"
  OnVolumeChange      -> "volumechange"
  OnWaiting           -> "waiting"


class CanHandleEvent handlerEs msg a where
  handleEvent :: EventAttr msg a -> a -> Event -> Eff handlerEs ()

instance Send msg :> handlerEs => CanHandleEvent handlerEs msg msg where
  handleEvent attr msg _ = sendMessage msg

instance Send msg :> handlerEs => CanHandleEvent handlerEs msg (MouseEvent -> msg) where
  handleEvent attr msg jsEvt = do let x = 0
                                      y = 0
                                  sendMessage $ msg (MouseEvent $ MousePosition x y)
