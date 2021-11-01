module FeyState.FeyState where

import {-# SOURCE #-} FeyState.GameState (GameState)
import Control.Monad.State.Lazy (StateT)

type FeyState a = StateT GameState IO a
