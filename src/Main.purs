-- Copyright 2016 Moritz Schulte <mtesseract@silverratio.net>
-- License: BSD3.

module Main where

import App.Dice (Action, State, view, update, init)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Debug.Trace (traceAny)
import DOM (DOM)
import Prelude (bind, return)
import Pux (App, start, renderToDOM)
import Signal.Channel (CHANNEL)
import Control.Monad.Eff.Random (RANDOM)

type AppEffects = (dom :: DOM)

-- | Entry point for the browser.
main :: State -> Eff (channel :: CHANNEL, err :: EXCEPTION, random :: RANDOM) (App State Action)
main state = do
  app <- start
    { initialState: init
    , update:
        -- | Logs all actions and states (removed in production builds).
        (\a s -> traceAny {action: a, state: s} (\_ -> update a s))
    , view: view
    , inputs: [] }

  renderToDOM "#app" app.html

  -- | Used by hot-reloading code in support/index.js
  return app
