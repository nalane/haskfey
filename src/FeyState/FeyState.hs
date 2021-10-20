{- |Contains the functions for using the FeyState monad
-}

module FeyState.FeyState (
    FeyState(..), fmap, pure, (<*>), (>>), (>>=), return, liftIO,
    getStateVar, setStateVar,
    runFeyState
) where

import FeyState.State

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy (StateT, evalStateT, get, modify)
import Control.Lens

-- |The FeyState monad itself
type FeyState a = StateT State IO a

-- |Returns the value referred to by the Lens
getStateVar :: ALens' State a -> FeyState a
getStateVar getter = do
    state <- get
    return (state^#getter)

-- |Set the state value referred to by the given Lens
setStateVar :: ALens' State a -> a -> FeyState ()
setStateVar setter val = modify $ \state -> storing setter val state

-- |Runs the FeyState and pushes it into the IO monad
runFeyState :: FeyState a -> State -> IO a
runFeyState = evalStateT