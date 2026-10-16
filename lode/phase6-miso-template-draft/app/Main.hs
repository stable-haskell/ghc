{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Miso
import           Miso.String (ms)

#ifdef WASM
-- FIXME: agent draft had `as JSaddle.Wasm` (dotted alias — invalid Haskell).
-- Use a single-conid alias or import unqualified.
import qualified Language.Javascript.JSaddle.Wasm as JSW
import           GHC.Wasm.Prim ()  -- brings JSString instances into scope
#else
import qualified Language.Javascript.JSaddle.Warp as Warp
#endif

-- | Model: a single counter.
type Model = Int

-- | Actions: increment / decrement.
data Action = AddOne | SubOne | NoOp deriving (Eq, Show)

-- | Pure update.
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubOne m = noEff (m - 1)
updateModel NoOp   m = noEff m

-- | Render.
viewModel :: Model -> View Action
viewModel n =
  div_ []
    [ h1_ [] [text "stable-haskell miso wasm counter"]
    , button_ [ onClick SubOne ] [ text "-" ]
    , span_   [] [ text (ms (show n)) ]
    , button_ [ onClick AddOne ] [ text "+" ]
    ]

-- | Shared app spec.
app :: App Model Action
app = App
  { initialAction = NoOp
  , model         = 0
  , update        = updateModel
  , view          = viewModel
  , events        = defaultEvents
  , subs          = []
  , mountPoint    = Nothing
  , logLevel      = Off
  }

#ifdef WASM
-- Wasm reactor entry point. The C function symbol must match the
-- linker --export=hs_start flag in myapp.cabal.
foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSW.run (startApp app)
#else
-- Native dev mode: serve via jsaddle-warp on :8080 so you can open the
-- app in a browser without the wasm toolchain (useful for fast iteration).
main :: IO ()
main = Warp.run 8080 (startApp app)
#endif
