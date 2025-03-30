module EffWeb.JSIO
  ( JSIO
  , evalJSIO
  ) where

import Effectful
import Effectful.Dispatch.Static

--------------------------------------------------------------------------------

data JSIO :: Effect
type instance DispatchOf JSIO = Static WithSideEffects
newtype instance StaticRep JSIO = MkJSIO ()

evalJSIO :: IOE :> es => Eff (JSIO : es) a -> Eff es a
evalJSIO = evalStaticRep (MkJSIO ())
