module EffWeb.DOM.Effect
  ( DOM
  , evalDOM
  ) where

import Effectful
import Effectful.Dispatch.Static

--------------------------------------------------------------------------------

data DOM  :: Effect
type instance DispatchOf DOM  = Static WithSideEffects
newtype instance StaticRep DOM = MkDOM ()


--
evalDOM :: IOE :> es => Eff (DOM : es) a -> Eff es a
evalDOM = evalStaticRep (MkDOM ())
