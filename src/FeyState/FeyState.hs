{- |Contains the functions for using the FeyState monad
-}

{-# LANGUAGE TemplateHaskell #-}

module FeyState.FeyState (
    FeyState(..), fmap, pure, (<*>), (>>), (>>=), return, liftIO,
    getStateVar, setStateVar,
    runFeyState,
    State, logFile, shaders, models, textures, keyState, config, window, gfxFunctions, gfxIValues,
    newState
) where

import Resources
import Config
import Graphics.InternalValues
import Graphics.GraphicsFunctions

import Data.Map

import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy (StateT, evalStateT, get, modify)
import Control.Lens
import Control.Concurrent.MVar

import Graphics.UI.GLFW as GLFW

-- |Persistent state of the engine
data State = State {
    _config :: Config,
    _logFile :: Handle,
    _window :: Maybe GLFW.Window,
    _keyState :: MVar (Map GLFW.Key Bool),

    --Resources
    _shaders :: Map String (Program, Int),
    _models :: Map String (Model, Int),
    _textures :: Map String (Texture, Int),

    -- Graphics engine internals
    _gfxIValues :: Maybe InternalValues,
    _gfxFunctions :: Maybe GraphicsFunctions
}
makeLenses ''State

-- |Given a path to a config file, create a new state for the Fey monad
newState :: String -> IO State
newState path = do
    rawCfg <- loadConfig path
    let cfg = case rawCfg of
            (Left e) -> error e
            (Right v) -> v
    
    fh <- openFile "log.txt" WriteMode
    k <- newMVar empty
    return $ State {
        _config = cfg,
        _logFile = fh,
        _window = Nothing,
        _keyState = k,

        _shaders = empty,
        _models = empty,
        _textures = empty,
        
        _gfxIValues = Nothing,
        _gfxFunctions = Nothing
    }



-- |The FeyState monad itself
type FeyState a = StateT State IO a

-- |Returns the value referred to by the Lens
getStateVar :: ALens' State a -> FeyState a
getStateVar getter = (^# getter) <$> get

-- |Set the state value referred to by the given Lens
setStateVar :: ALens' State a -> a -> FeyState ()
setStateVar setter val = modify (& setter #~ val)

-- |Runs the FeyState and pushes it into the IO monad
runFeyState :: FeyState a -> State -> IO a
runFeyState = evalStateT

