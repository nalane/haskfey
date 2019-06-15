{- |Contains functions for loading and unloading resources
-}

module FeyState.ResourceHandler (
    Resource(..), unwrap,
    loadShader, unloadShader,
    loadModel, unloadModel
) where

import Resources
import FeyState.FeyState
import FeyState.State
import FeyState.Logging

import Data.Function (on)
import Data.List
import Data.Maybe
import qualified Data.Map as M

-- |A wrapper for resources containing the key and the resource itself
data Resource a = Resource String a

resKey :: Resource a -> String
resKey (Resource k _) = k

-- |Unwraps a resource from the Resource data type
unwrap :: Resource a -> a
unwrap (Resource _ v) = v

shaderKey :: [(ShaderType, FilePath)] -> String
shaderKey = concatMap snd . sortBy (flip compare `on` fst)

-- |Returns a shader specified by the files in the given list.
-- If the shader has already been loaded, the shader is taken from cache
loadShader :: [(ShaderType, FilePath)] -> FeyState (Resource Program)
loadShader list = do
    shadMap <- getStateVar shaders

    let key = shaderKey list
    let mapVal = M.lookup key shadMap

    case mapVal of
        Just (val, count) -> do
            setStateVar shaders $
                M.adjust (\(prog, count) -> (prog, count + 1)) key shadMap
            return $ Resource key val
        Nothing -> do
            recordLog ("Loading shader " ++ key)
            prog <- liftIO $ createShaderProgram list
            setStateVar shaders $ M.insert key (prog, 1) shadMap
            return $ Resource key prog

-- |Given a shader, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadShader :: Resource Program -> FeyState ()
unloadShader res = do
    shadMap <- getStateVar shaders

    let key = resKey res
    let newMap = M.adjust (\(item, count) -> (item, count - 1)) key shadMap
    let val = M.lookup key newMap

    case val of
        Just (prog, 0) -> do
            recordLog ("Unloading shader " ++ key)
            liftIO $ destroyShaderProgram prog
            setStateVar shaders $ M.delete key newMap
        _ -> setStateVar shaders newMap

-- |Returns a model specified by the given file
-- If the model has already been loaded, the model is taken from cache
loadModel :: FilePath -> FeyState (Resource Model)
loadModel key = do
    modelMap <- getStateVar models
    let mapVal = M.lookup key modelMap

    case mapVal of
        Just (val, count) -> do
            setStateVar models $
                M.adjust (\(model, count) -> (model, count + 1)) key modelMap
            return $ Resource key val
        Nothing -> do
            recordLog ("Loading model " ++ key)
            model <- liftIO $ createModel key
            setStateVar models $ M.insert key (model, 1) modelMap
            return $ Resource key model

-- |Given a model, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadModel :: Resource Model -> FeyState ()
unloadModel res = do
    modelMap <- getStateVar models

    let key = resKey res
    let newMap = M.adjust (\(item, count) -> (item, count - 1)) key modelMap
    let val = M.lookup key newMap

    case val of
        Just (prog, 0) -> do
            recordLog ("Unloading model " ++ key)
            liftIO $ destroyModel prog
            setStateVar models $ M.delete key newMap
        _ -> setStateVar models newMap