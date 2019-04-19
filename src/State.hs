{-| Contains functions for creating and interacting with
the persistent state of the engine
-}

{-# LANGUAGE TemplateHaskell #-}

module State (
    logPath, logFile, window, width, height,
    FeyState(..), fmap, pure, (<*>), (>>), (>>=), return, runFeyState,
    getStateVar, setStateVar, execute,
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
    _shaders :: Map String (Program, Int)
}

makeLenses ''State



newtype FeyState a = FeyState (State -> IO (State, a))

instance Functor FeyState where
    fmap f (FeyState s) = FeyState (\state -> do
        (newState, val) <- s state
        return (newState, f val))

instance Applicative FeyState where
    pure val = FeyState (\state -> return (state, val))
    (<*>) (FeyState lhs) (FeyState rhs) = FeyState (\state -> do
        (newState, f) <- lhs state
        (finalState, val) <- rhs newState
        return (finalState, f val))

instance Monad FeyState where
    (>>) (FeyState lhs) (FeyState rhs) = FeyState (\state -> do
        (newState, _) <- lhs state
        rhs newState)

    (>>=) (FeyState lhs) f = FeyState (\state -> do
        (newState, val) <- lhs state
        let (FeyState rhs) = f val
        rhs newState)

    return val = FeyState (\state -> return (state, val))

getStateVar :: Getting a State a -> FeyState a
getStateVar getter = FeyState (\state -> return (state, state^.getter))

setStateVar :: ASetter State State a b -> b -> FeyState ()
setStateVar setter val = FeyState (\state -> return (set setter val state, ()))

execute :: IO a -> FeyState a
execute action = FeyState (\state -> do
    val <- action
    return (state, val))



newState :: String -> IO State
newState path = do
    fh <- openFile path WriteMode
    return $ State (Just path) (Just fh) Nothing Nothing Nothing empty

-- |Runs the FeyState and pushes it into the IO monad
runFeyState :: String -> FeyState a -> IO a
runFeyState path (FeyState f) = do
    state <- newState path
    (_, res) <- f state
    return res



-- |Writes to the log
recordLog :: String -> FeyState ()
recordLog msg = do
    maybeFile <- getStateVar logFile
    execute $ forM_ maybeFile $ flip hPutStrLn msg

-- |Terminates the logging system
endLogging :: FeyState ()
endLogging = do
    maybeFile <- getStateVar logFile
    execute $ forM_ maybeFile hClose
    setStateVar logPath Nothing 
    setStateVar logFile Nothing



-- |Returns a shader specified by the files in the given list.
-- If the shader has already been loaded, the shader is taken from cache
loadShader :: [(ShaderType, FilePath)] -> FeyState Program
loadShader list = do
    let sortedList = sortBy (\(t1, _) (t2, _) -> compare t1 t2) list
    let key = concatMap snd sortedList

    shadMap <- getStateVar shaders
    let mapVal = M.lookup key shadMap

    case mapVal of
        Just (val, count) -> do
            setStateVar shaders $ adjust (\(prog, count) -> (prog, count + 1)) key shadMap
            return val
        Nothing -> do
            recordLog ("Loading shader " ++ key)
            prog <- execute $ createShaderProgram sortedList
            setStateVar shaders $ M.insert key (prog, 1) shadMap
            return prog

-- |Given a shader, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadShader :: [(ShaderType, FilePath)] -> FeyState ()
unloadShader list = do
    let sortedList = sortBy (\(t1, _) (t2, _) -> compare t1 t2) list
    let key = concatMap snd sortedList

    shadMap <- getStateVar shaders

    let newMap = adjust (\(item, count) -> (item, count - 1)) key shadMap
    let val = M.lookup key newMap

    case val of
        Just (prog, 0) -> do
            recordLog ("Unloading shader " ++ key)
            execute $ deleteObjectName prog
            setStateVar shaders $ M.delete key newMap
        _ -> setStateVar shaders newMap