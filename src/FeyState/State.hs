{-| This structure contains the internal state of the engine
-}

{-# LANGUAGE TemplateHaskell #-}

module FeyState.State (
    State(..), config, logFile, window, keyState, shaders, models,
    newState
) where

import Resources

import FeyState.Config

import Data.Maybe
import Data.Map
import Data.Bool

import System.IO
import Control.Lens (makeLenses, (^.))
import Control.Concurrent.MVar

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

import Text.Parsec (spaces, many, noneOf)
import Text.Parsec.ByteString
import Text.Parsec.Number

-- |Persistent state of the engine
data State = State {
    _config :: Config,
    _logFile :: Handle,
    _window :: Maybe GLFW.Window,
    _keyState :: MVar (Map GLFW.Key Bool),

    --Resources
    _shaders :: Map String (Program, Int),
    _models :: Map String (Model, Int)
}

makeLenses ''State

-- |Given a path to a config file, create a new state for the Fey monad
newState :: String -> IO State
newState path = do
    cfg <- loadConfig path
    
    fh <- openFile "log.txt" WriteMode
    k <- newMVar empty
    return $ State cfg fh Nothing k empty empty