{- |Contains functions for loading and unloading resources
-}

module FeyState.ResourceHandler (
    Resource, unwrap,
    loadShader, unloadShader,
    loadModel, unloadModel,
    loadTexture, unloadTexture
) where

import Resources
import Graphics

import FeyState.FeyState
import FeyState.Logging
import FeyState.GameState

import Control.Lens

import Data.Function (on)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.Error.Class (Error(noMsg))

-- |A wrapper for resources containing the key and the resource itself
data Resource a = Resource String a

resKey :: Resource a -> String
resKey (Resource k _) = k

-- |Unwraps a resource from the Resource data type
unwrap :: Resource a -> a
unwrap (Resource _ v) = v



shaderKey :: [(ShaderType, FilePath)] -> String
shaderKey = concatMap snd . sortBy (flip compare `on` fst)

loadResource :: 
    ALens' GameState (M.Map String (a, Int)) ->
    String -> 
    String ->
    IO (Either String a) -> 
    FeyState (Resource a)
loadResource mapLens key resName creator = do
    resMap <- getStateVar mapLens
    let mapVal = M.lookup key resMap

    case mapVal of
        Just (val, count) -> do
            setStateVar mapLens $
                M.adjust (\(res, count) -> (res, count + 1)) key resMap
            return $ Resource key val
        Nothing -> do
            recordLog ("Loading " ++ resName ++ " " ++ key)
            res <- liftIO creator
            case res of
                (Left msg) -> do
                    recordLog $ "Loading resource " ++ key ++ " failed with message: " ++ msg
                    error "Loading resource failed" 
                (Right r) -> do
                    setStateVar mapLens $ M.insert key (r, 1) resMap
                    return $ Resource key r

unloadResource :: 
    ALens' GameState (M.Map String (a, Int)) ->
    String -> 
    String ->
    (a -> IO ()) -> 
    FeyState ()
unloadResource mapLens key resName destructor = do
    resMap <- getStateVar mapLens

    let newMap = M.adjust (\(item, count) -> (item, count - 1)) key resMap
    let val = M.lookup key newMap

    case val of
        Just (res, 0) -> do
            recordLog ("Unloading " ++ resName ++ " " ++ key)
            liftIO $ destructor res
            setStateVar mapLens $ M.delete key newMap
        _ -> setStateVar mapLens newMap

-- |Returns a shader specified by the files in the given list.
-- If the shader has already been loaded, the shader is taken from cache
loadShader :: [(ShaderType, FilePath)] -> FeyState (Resource Program)
loadShader list = loadResource shaders key "shader" creator where
    key = shaderKey list
    creator = createShaderProgram list

-- |Given a shader, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadShader :: Resource Program -> FeyState ()
unloadShader res = unloadResource shaders key "shader" destroyShaderProgram where
    key = resKey res

-- |Returns a model specified by the given file
-- If the model has already been loaded, the model is taken from cache
loadModel :: FilePath -> FeyState (Resource Model)
loadModel key = loadResource models key "model" $ createModel key

-- |Given a model, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadModel :: Resource Model -> FeyState ()
unloadModel res = unloadResource models key "model" destroyModel where
    key = resKey res

-- |Returns a texture specified by the given file
-- If the texture has already been loaded, it is taken from cache
loadTexture :: FilePath -> FeyState (Resource Texture)
loadTexture key = loadResource textures key "texture" $ createTexture key

-- |Given a texture, decrements its reference count and deletes it from cache
-- if its reference count reaches zero.
unloadTexture :: Resource Texture -> FeyState ()
unloadTexture res = unloadResource textures key "texture" destroyTexture where
    key = resKey res