{-| Contains functions for creating and interacting with
the persistent state of the engine
-}

{-# LANGUAGE TemplateHaskell #-}

module State (
    State(..), logPath, logFile, newState, window, width, height,
    recordLog, endLogging,
    loadShader, unloadShader
) where

import System.IO
import Graphics.UI.GLFW as GLFW

import Data.List
import Data.Maybe
import Data.Map as M

import Control.Lens
import Control.Monad
import Control.Concurrent.MVar

import Graphics.Shader

import Graphics.Rendering.OpenGL

-- |Persistent state of the engine
data State = State {
    _logPath :: Maybe String,
    _logFile :: Maybe Handle,
    _window :: Maybe GLFW.Window,
    _width :: Maybe Int,
    _height :: Maybe Int,

    --Resources
    _shaders :: MVar (Map String (Program, Int))
}

makeLenses ''State

-- |Creates a blank state
newState :: String -> IO State
newState path = do
    mShader <- newMVar empty
    fh <- openFile path WriteMode
    return $ State (Just path) (Just fh) Nothing Nothing Nothing mShader



-- |Writes to the log
recordLog :: State -> String -> IO ()
recordLog state msg = do
    let maybeFile = state^.logFile
    forM_ maybeFile $ flip hPutStrLn msg

-- |Terminates the logging system
endLogging :: State -> IO State
endLogging state = do
    let maybeFile = state^.logFile
    forM_ maybeFile hClose
    return $ set logPath Nothing $ set logFile Nothing state



-- |Returns a shader specified by the files in the given list.
-- If the shader has already been loaded, the shader is taken from cache
loadShader :: State -> [(ShaderType, FilePath)] -> IO Program
loadShader state list = do
    let sortedList = sortBy (\(t1, _) (t2, _) -> compare t1 t2) list
    let key = concatMap snd sortedList

    shadMap <- readMVar (state^.shaders)
    let mapVal = M.lookup key shadMap

    case mapVal of
        Just (val, count) -> do
            modifyMVar_ (state^.shaders)
                (return . adjust (\(prog, count) -> (prog, count + 1)) key)
            return val
        Nothing -> do
            recordLog state ("Loading shader " ++ key)
            prog <- createShaderProgram sortedList
            modifyMVar_ (state^.shaders) (return . M.insert key (prog, 1))
            return prog

-- |Given a shader, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadShader :: State -> [(ShaderType, FilePath)] -> IO ()
unloadShader state list = do
    let sortedList = sortBy (\(t1, _) (t2, _) -> compare t1 t2) list
    let key = concatMap snd sortedList

    modifyMVar_ (state^.shaders) $ \orig -> do
        let newMap = adjust (\(item, count) -> (item, count - 1)) key orig
        let val = M.lookup key newMap

        case val of
            Just (prog, 0) -> do
                recordLog state ("Unloading shader " ++ key)
                deleteObjectName prog
                return $ M.delete key newMap
            Just (_, _) -> return newMap
            Nothing -> return newMap