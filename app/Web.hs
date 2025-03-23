module Main where

import GHC.Wasm.Prim

--------------------------------------------------------------------------------

foreign export javascript "hs_start"
  main :: IO ()


foreign import javascript unsafe "console.log($1)"
  jsLog :: JSString -> IO ()


main :: IO ()
main = jsLog $ toJSString "woei"
