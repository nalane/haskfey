{- |Contains the functions for using the FeyState monad
-}

module FeyState.FeyState (
    FeyState(..), fmap, pure, (<*>), (>>), (>>=), return, liftIO,
    getStateVar, setStateVar,
    runFeyState
) where

import Resources
import Config
import FeyState.GameState

import Data.Map

import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy (StateT, evalStateT, get, modify)
import Control.Lens
import Control.Concurrent.MVar

-- |The FeyState monad itself
type FeyState a = StateT GameState IO a

-- |Returns the value referred to by the Lens
getStateVar :: ALens' GameState a -> FeyState a
getStateVar getter = (^# getter) <$> get

-- |Set the state value referred to by the given Lens
setStateVar :: ALens' GameState a -> a -> FeyState ()
setStateVar setter val = modify (& setter #~ val)

-- |Runs the FeyState and pushes it into the IO monad
runFeyState :: FeyState a -> GameState -> IO a
runFeyState = evalStateT

