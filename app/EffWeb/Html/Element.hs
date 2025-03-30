{-# LANGUAGE OverloadedStrings #-}
module EffWeb.Html.Element where

import           Data.String (IsString(..))
import           Data.Text (Text)
-- import qualified Data.Text as Text

--------------------------------------------------------------------------------

data HtmlElement = A
                 | Abbr
                 | Address
                 | Area
                 | Article
                 | Aside
                 | Audio
                 | B
                 | Base
                 | Bdi
                 | Bdo
                 | Blockquote
                 | Body
                 | Br
                 | Button
                 | Canvas
                 | Caption
                 | Cite
                 | Code
                 | Col
                 | Colgroup
                 | Data
                 | Datalist
                 | Dd
                 | Del
                 | Details
                 | Dfn
                 | Dialog
                 | Div
                 | Dl
                 | Dt
                 | Em
                 | Embed
                 | Fencedframe -- Experimental
                 | Fieldset
                 | Figcaption
                 | Figure
                 | Footer
                 | Form
                 | H1
                 | H2
                 | H3
                 | H4
                 | H5
                 | H6
                 -- | Head
                 | Header
                 | Hgroup
                 | Hr
                 -- | Html
                 | I
                 | Iframe
                 | Img
                 | Input
                 | Ins
                 | Kbd
                 | Label
                 | Legend
                 | Li
                 | Link
                 | Main
                 | Map
                 | Mark
                 | Menu
                 | Meta
                 | Meter
                 | Nav
                 | Noscript
                 | Object
                 | Ol
                 | Optgroup
                 | Option
                 | Output
                 | P
                 | Picture
                 | Pre
                 | Progress
                 | Q
                 | Rp
                 | Rt
                 | Ruby
                 | S
                 | Samp
                 | Script
                 | Search
                 | Section
                 | Select
                 | Slot
                 | Small
                 | Source
                 | Span
                 | Strong
                 | Style
                 | Sub
                 | Summary
                 | Sup
                 | Table
                 | Tbody
                 | Td
                 | Template
                 | Textarea
                 | Tfoot
                 | Th
                 | Thead
                 | Time
                 | Title
                 | Tr
                 | Track
                 | U
                 | Ul
                 | Var
                 | Video
                 | Wbr
                 deriving (Show,Eq,Ord,Enum,Bounded)

elementNameOf :: HtmlElement -> ElementName
elementNameOf = ElementName . \case
  A           -> "a"
  Abbr        -> "abbr"
  Address     -> "address"
  Area        -> "area"
  Article     -> "article"
  Aside       -> "aside"
  Audio       -> "audio"
  B           -> "b"
  Base        -> "base"
  Bdi         -> "bdi"
  Bdo         -> "bdo"
  Blockquote  -> "blockquote"
  Body        -> "body"
  Br          -> "br"
  Button      -> "button"
  Canvas      -> "canvas"
  Caption     -> "caption"
  Cite        -> "cite"
  Code        -> "code"
  Col         -> "col"
  Colgroup    -> "colgroup"
  Data        -> "data"
  Datalist    -> "datalist"
  Dd          -> "dd"
  Del         -> "del"
  Details     -> "details"
  Dfn         -> "dfn"
  Dialog      -> "dialog"
  Div         -> "div"
  Dl          -> "dl"
  Dt          -> "dt"
  Em          -> "em"
  Embed       -> "embed"
  Fencedframe -> "fencedframe"
  Fieldset    -> "fieldset"
  Figcaption  -> "figcaption"
  Figure      -> "figure"
  Footer      -> "footer"
  Form        -> "form"
  H1          -> "h1"
  H2          -> "h2"
  H3          -> "h3"
  H4          -> "h4"
  H5          -> "h5"
  H6          -> "h6"
  Header      -> "header"
  Hgroup      -> "hgroup"
  Hr          -> "hr"
  I           -> "i"
  Iframe      -> "iframe"
  Img         -> "img"
  Input       -> "input"
  Ins         -> "ins"
  Kbd         -> "kbd"
  Label       -> "label"
  Legend      -> "legend"
  Li          -> "li"
  Link        -> "link"
  Main        -> "main"
  Map         -> "map"
  Mark        -> "mark"
  Menu        -> "menu"
  Meta        -> "meta"
  Meter       -> "meter"
  Nav         -> "nav"
  Noscript    -> "noscript"
  Object      -> "object"
  Ol          -> "ol"
  Optgroup    -> "optgroup"
  Option      -> "option"
  Output      -> "output"
  P           -> "p"
  Picture     -> "picture"
  Pre         -> "pre"
  Progress    -> "progress"
  Q           -> "q"
  Rp          -> "rp"
  Rt          -> "rt"
  Ruby        -> "ruby"
  S           -> "s"
  Samp        -> "samp"
  Script      -> "script"
  Search      -> "search"
  Section     -> "section"
  Select      -> "select"
  Slot        -> "slot"
  Small       -> "small"
  Source      -> "source"
  Span        -> "span"
  Strong      -> "strong"
  Style       -> "style"
  Sub         -> "sub"
  Summary     -> "summary"
  Sup         -> "sup"
  Table       -> "table"
  Tbody       -> "tbody"
  Td          -> "td"
  Template    -> "template"
  Textarea    -> "textarea"
  Tfoot       -> "tfoot"
  Th          -> "th"
  Thead       -> "thead"
  Time        -> "time"
  Title       -> "title"
  Tr          -> "tr"
  Track       -> "track"
  U           -> "u"
  Ul          -> "ul"
  Var         -> "var"
  Video       -> "video"
  Wbr         -> "wbr"

newtype ElementName = ElementName Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)
