{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#ifdef WASM
import GHC.Wasm.Prim ()       -- JSString instances; required even unused
#endif

import Miso
import Miso.Html.Element (div_, h1_, button_, span_)
import Miso.Html.Event   (onClick)
import Miso.String       (ms)
import Control.Monad.State (modify)

-- | The model is just a counter.
type Model = Int

data Action = AddOne | SubOne | NoOp
  deriving (Show, Eq)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = startApp defaultEvents app

-- | App = Component ROOT () Model Action (per miso 1.11).
-- `component` is the smart constructor; `vcomp` is its alias.
app :: App Model Action
app = component (0 :: Model) updateModel viewModel
  where
    updateModel :: Action -> Effect parent props Model Action
    updateModel AddOne = modify (+1)
    updateModel SubOne = modify (subtract 1)
    updateModel NoOp   = pure ()

    viewModel :: props -> Model -> View Model Action
    viewModel _ n = div_ []
      [ h1_     [] [ text "stable-haskell miso wasm counter" ]
      , button_ [ onClick SubOne ] [ text "-" ]
      , span_   [] [ text (ms (show n)) ]
      , button_ [ onClick AddOne ] [ text "+" ]
      ]
