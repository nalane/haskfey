{-| Contains functions for creating and interacting with
the persistent state of the engine
-}

module FeyState (
    module FeyState.State,
    module FeyState.FeyState,
    module FeyState.Logging,
    loadShader, unloadShader
) where

import FeyState.State
import FeyState.FeyState
import FeyState.Logging

import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Map as M

import Graphics.Shader
import Graphics.Rendering.OpenGL

shaderKey :: [(ShaderType, FilePath)] -> String
shaderKey = concatMap snd . sortBy (flip compare `on` fst)

-- |Returns a shader specified by the files in the given list.
-- If the shader has already been loaded, the shader is taken from cache
loadShader :: [(ShaderType, FilePath)] -> FeyState Program
loadShader list = do
    shadMap <- getStateVar shaders

    let key = shaderKey list
    let mapVal = M.lookup key shadMap

    case mapVal of
        Just (val, count) -> do
            setStateVar shaders $ adjust (\(prog, count) -> (prog, count + 1)) key shadMap
            return val
        Nothing -> do
            recordLog ("Loading shader " ++ key)
            prog <- liftIO $ createShaderProgram list
            setStateVar shaders $ M.insert key (prog, 1) shadMap
            return prog

-- |Given a shader, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadShader :: [(ShaderType, FilePath)] -> FeyState ()
unloadShader list = do
    shadMap <- getStateVar shaders

    let key = shaderKey list
    let newMap = adjust (\(item, count) -> (item, count - 1)) key shadMap
    let val = M.lookup key newMap

    case val of
        Just (prog, 0) -> do
            recordLog ("Unloading shader " ++ key)
            liftIO $ deleteObjectName prog
            setStateVar shaders $ M.delete key newMap
        _ -> setStateVar shaders newMap