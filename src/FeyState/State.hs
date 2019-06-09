{-| This structure contains the internal state of the engine
-}

{-# LANGUAGE TemplateHaskell #-}

module FeyState.State (
    State(..), logPath, logFile, window, width, height, keyState, shaders, models,
    newState
) where

import Resources

import Data.Maybe
import Data.Map

import System.IO
import Control.Lens
import Control.Concurrent.MVar

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

-- |Persistent state of the engine
data State = State {
    _logPath :: Maybe String,
    _logFile :: Maybe Handle,
    _window :: Maybe GLFW.Window,
    _width :: Maybe Int,
    _height :: Maybe Int,

    _keyState :: MVar (Map GLFW.Key Bool),

    --Resources
    _shaders :: Map String (Program, Int),
    _models :: Map String (Model, Int)
}

makeLenses ''State

newState :: String -> IO State
newState path = do
    fh <- openFile path WriteMode
    k <- newMVar empty
    return $ State (Just path) (Just fh) Nothing Nothing Nothing k empty empty