module FeyState.FeyState where

import FeyState.GameState
import Control.Monad.State.Lazy (StateT)
import Control.Lens

type FeyState a = StateT GameState IO a
getStateVar :: ALens' GameState a -> FeyState a
setStateVar :: ALens' GameState a -> a -> FeyState ()