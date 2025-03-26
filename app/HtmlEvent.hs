{-# LANGUAGE OverloadedStrings #-}
module HtmlEvent
  ( EventAttr(..)
  , asEventType
  ) where

import FFI.Types


data EventAttr = OnAbort
               | OnAutocomplete
               | OnAutocompleteerror
               | OnBlur
               | OnCancel
               | OnCanplay
               | OnCanplaythrough
               | OnChange
               | OnClick
               | OnClose
               | OnContextmenu
               | OnCuechange
               | OnDblclick
               | OnDrag
               | OnDragend
               | OnDragenter
               | OnDragleave
               | OnDragover
               | OnDragstart
               | OnDrop
               | OnDurationchange
               | OnEmptied
               | OnEnded
               | OnError
               | OnFocus
               | OnInput
               | OnInvalid
               | OnKeydown
               | OnKeypress
               | OnKeyup
               | OnLoad
               | OnLoadeddata
               | OnLoadedmetadata
               | OnLoadstart
               | OnMousedown
               | OnMouseenter
               | OnMouseleave
               | OnMousemove
               | OnMouseout
               | OnMouseover
               | OnMouseup
               | OnMousewheel
               | OnPause
               | OnPlay
               | OnPlaying
               | OnProgress
               | OnRatechange
               | OnReset
               | OnResize
               | OnScroll
               | OnSeeked
               | OnSeeking
               | OnSelect
               | OnShow
               | OnSort
               | OnStalled
               | OnSubmit
               | OnSuspend
               | OnTimeupdate
               | OnToggle
               | OnVolumechange
               | OnWaiting
               deriving (Show,Eq,Ord,Enum)

-- | Render as event type
asEventType :: EventAttr -> EventType
asEventType = EventType . \case
  OnAbort             -> "abort"
  OnAutocomplete      -> "autocomplete"
  OnAutocompleteerror -> "autocompleteerror"
  OnBlur              -> "blur"
  OnCancel            -> "cancel"
  OnCanplay           -> "canplay"
  OnCanplaythrough    -> "canplaythrough"
  OnChange            -> "change"
  OnClick             -> "click"
  OnClose             -> "close"
  OnContextmenu       -> "contextmenu"
  OnCuechange         -> "cuechange"
  OnDblclick          -> "dblclick"
  OnDrag              -> "drag"
  OnDragend           -> "dragend"
  OnDragenter         -> "dragenter"
  OnDragleave         -> "dragleave"
  OnDragover          -> "dragover"
  OnDragstart         -> "dragstart"
  OnDrop              -> "drop"
  OnDurationchange    -> "durationchange"
  OnEmptied           -> "emptied"
  OnEnded             -> "ended"
  OnError             -> "error"
  OnFocus             -> "focus"
  OnInput             -> "input"
  OnInvalid           -> "invalid"
  OnKeydown           -> "keydown"
  OnKeypress          -> "keypress"
  OnKeyup             -> "keyup"
  OnLoad              -> "load"
  OnLoadeddata        -> "loadeddata"
  OnLoadedmetadata    -> "loadedmetadata"
  OnLoadstart         -> "loadstart"
  OnMousedown         -> "mousedown"
  OnMouseenter        -> "mouseenter"
  OnMouseleave        -> "mouseleave"
  OnMousemove         -> "mousemove"
  OnMouseout          -> "mouseout"
  OnMouseover         -> "mouseover"
  OnMouseup           -> "mouseup"
  OnMousewheel        -> "mousewheel"
  OnPause             -> "pause"
  OnPlay              -> "play"
  OnPlaying           -> "playing"
  OnProgress          -> "progress"
  OnRatechange        -> "ratechange"
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
  OnTimeupdate        -> "timeupdate"
  OnToggle            -> "toggle"
  OnVolumechange      -> "volumechange"
  OnWaiting           -> "waiting"
