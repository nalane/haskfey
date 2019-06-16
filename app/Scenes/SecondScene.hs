{-# LANGUAGE TemplateHaskell #-}

module Scenes.SecondScene (
    SecondScene(..), initScene
) where

import Lib

import Graphics.UI.GLFW as GLFW

import Data.Maybe
import Data.IORef
import Data.StateVar

import Control.Monad
import Control.Lens

data SecondScene = SecondScene {
    _prog :: Resource Program,
    _model :: Resource Model,
    _ubo :: UniformLocation,
    _time :: IORef Float,
    _cam :: FeyMatrix,
    _proj :: FeyMatrix
}

makeLenses ''SecondScene

initScene :: FeyState Scene
initScene = do
    shaderProg <- loadShader [
        (FragmentShader, "feyData/shaders/bare/bare.frag"),
        (VertexShader, "feyData/shaders/bare/bare.vert")]
    liftIO $ setShader $ unwrap shaderProg

    m <- loadModel "feyData/library/cube.fey.model"

    w <- getStateVar (config.width)
    h <- getStateVar (config.height)
    let cam = camera [0, 0, 0.5] [0, 0, 0] [0, 1, 0]
    let proj = orthographic w h
    location <- liftIO $ getUniformLocation (unwrap shaderProg) "mvpMatrix"

    timeVar <- liftIO $ newIORef 0
    let ms = SecondScene {
        _prog = shaderProg,
        _model = m,
        _ubo = location,
        _time = timeVar,
        _cam = cam,
        _proj = proj }
    return $ Scene (updateSecondScene ms) (drawSecondScene ms) (endSecondScene ms)

updateSecondScene :: SecondScene -> FeyState (Maybe String)
updateSecondScene ms = do
    win <- fromJust <$> getStateVar window
    shouldEnd <- isKeyPressed GLFW.Key'Escape
    when shouldEnd $ liftIO $ GLFW.setWindowShouldClose win True

    (ms^.time) $~ (+0.01)

    swapScenes <- isKeyPressed GLFW.Key'Space
    if swapScenes
        then return $ Just "main" 
        else return Nothing

drawSecondScene :: SecondScene -> FeyState ()
drawSecondScene ms = do
    liftIO $ setShader $ unwrap (ms^.prog)

    t <- get (ms^.time)
    liftIO $ setUniformMatrix (ms^.ubo) $
        multiply (ms^.proj) $
        multiply (ms^.cam) $
        multiply (rotate t [1, 1, 1]) $ 
        scale 0.1
        
    liftIO $ drawModel $ unwrap (ms^.model)

endSecondScene :: SecondScene -> FeyState ()
endSecondScene ms = do
    unloadShader (ms^.prog)
    unloadModel (ms^.model)