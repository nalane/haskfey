{-# LANGUAGE TemplateHaskell #-}

module FeyState.GameState (
    GameState, logFile, shaders, models, textures, keyState, config, window, gfxFunctions, gfxIValues,
    newState
) where

import Config
import Resources
import FeyState.ComponentDatabase

import Graphics.InternalValues
import Graphics.GraphicsFunctions
import Graphics.Texture

import Data.Map
import Data.Default
import System.IO
import Control.Concurrent.MVar
import Control.Lens

import Graphics.UI.GLFW as GLFW

-- |Persistent state of the engine
data GameState = GameState {
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
    _gfxFunctions :: Maybe GraphicsFunctions,

    _componentDatabase :: ComponentDatabase
}
makeLenses ''GameState

-- |Given a path to a config file, create a new state for the Fey monad
newState :: String -> IO GameState
newState path = do
    rawCfg <- loadConfig path
    let cfg = case rawCfg of
            Left e -> error e
            Right v -> v
    
    fh <- openFile "log.txt" WriteMode
    k <- newMVar empty
    return $ GameState {
        _config = cfg,
        _logFile = fh,
        _window = Nothing,
        _keyState = k,

        _shaders = empty,
        _models = empty,
        _textures = empty,
        
        _gfxIValues = Nothing,
        _gfxFunctions = Nothing,

        _componentDatabase = def
    }