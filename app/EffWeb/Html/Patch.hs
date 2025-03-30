module EffWeb.Html.Patch
  (

  ) where

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
