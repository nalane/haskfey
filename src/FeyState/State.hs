{-| This structure contains the internal state of the engine
-}

{-# LANGUAGE TemplateHaskell #-}

module FeyState.State (
    State, config, logFile, window, keyState, shaders, models, textures, gfxIValues, gfxFunctions,
    newState
) where

import Resources
import Config

import Data.Maybe
import Data.Map

import System.IO
import Control.Lens (makeLenses)
import Control.Concurrent.MVar
import Control.Monad.IO.Class

import Graphics.UI.GLFW as GLFW

import Graphics.InternalValues
import Graphics.GraphicsFunctions

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