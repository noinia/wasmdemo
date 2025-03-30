{-# LANGUAGE OverloadedStrings #-}
module HtmlEvent
  ( EventAttr(..)
  , asEventType
  ) where

import FFI.Types


data EventAttr = OnAbort
               | OnAutoComplete
               | OnAutoCompleteError
               | OnBlur
               | OnCancel
               | OnCanplay
               | OnCanplayThrough
               | OnChange
               | OnClick
               | OnClose
               | OnContextMenu
               | OnCueChange
               | OnDblClick
               | OnDrag
               | OnDragEnd
               | OnDragEnter
               | OnDragLeave
               | OnDragOver
               | OnDragStart
               | OnDrop
               | OnDurationChange
               | OnEmptied
               | OnEnded
               | OnError
               | OnFocus
               | OnInput
               | OnInvalid
               | OnKeyDown
               | OnKeyPress
               | OnKeyUp
               | OnLoad
               | OnLoadedData
               | OnLoadedMetaData
               | OnLoadStart
               | OnMouseDown
               | OnMouseEnter
               | OnMouseLeave
               | OnMouseMove
               | OnMouseOut
               | OnMouseOver
               | OnMouseUp
               | OnMouseWheel
               | OnPause
               | OnPlay
               | OnPlaying
               | OnProgress
               | OnRateChange
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
               | OnTimeUpdate
               | OnToggle
               | OnVolumeChange
               | OnWaiting
               deriving (Show,Eq,Ord,Enum)

-- | Render as event type
asEventType :: EventAttr -> EventType
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
