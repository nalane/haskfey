{-# LANGUAGE TemplateHaskell #-}

module Scenes.MainScene (
    MainScene(..), initScene
) where

import Lib

import Graphics.UI.GLFW as GLFW

import Data.Maybe
import Data.Word
import Data.IORef
import Data.StateVar

import Control.Monad
import Control.Lens (makeLenses, (^.))

data MainScene = MainScene {
    _prog :: Resource Program,
    _model :: Resource Model,
    _ubo :: UniformLocation,
    _time :: IORef Float,
    _cam :: FeyMatrix,
    _proj :: FeyMatrix
}

makeLenses ''MainScene

initScene :: FeyState Scene
initScene = do
    shaderProg <- loadShader [
        (FragmentShader, "feyData/shaders/bare/bare.frag"),
        (VertexShader, "feyData/shaders/bare/bare.vert")]
    liftIO $ setShader $ unwrap shaderProg

    m <- loadModel "feyData/library/monkey.fey.model"

    w <- fromJust <$> getStateVar width
    h <- fromJust <$> getStateVar height
    let cam = camera [0, 0, 0.5] [0, 0, 0] [0, 1, 0]
    let proj = orthographic w h
    location <- liftIO $ getUniformLocation (unwrap shaderProg) "mvpMatrix"

    timeVar <- liftIO $ newIORef 0
    let ms = MainScene {
        _prog = shaderProg,
        _model = m,
        _ubo = location,
        _time = timeVar,
        _cam = cam,
        _proj = proj }
    return $ Scene (updateMainScene ms) (drawMainScene ms) (endMainScene ms)

updateMainScene :: MainScene -> FeyState (Maybe String)
updateMainScene ms = do
    win <- fromJust <$> getStateVar window
    shouldEnd <- isKeyPressed GLFW.Key'Escape
    when shouldEnd $ liftIO $ GLFW.setWindowShouldClose win True

    (ms^.time) $~ (+0.01)

    swapScenes <- isKeyPressed GLFW.Key'Space
    if swapScenes
        then return $ Just "second" 
        else return Nothing

drawMainScene :: MainScene -> FeyState ()
drawMainScene ms = do
    liftIO $ setShader $ unwrap (ms^.prog)

    t <- get (ms^.time)
    liftIO $ setUniformMatrix (ms^.ubo) $
        multiply (ms^.proj) $
        multiply (ms^.cam) $
        rotate t [0, 0, 1]
        
    liftIO $ drawModel $ unwrap (ms^.model)

endMainScene :: MainScene -> FeyState ()
endMainScene ms = do
    unloadShader (ms^.prog)
    unloadModel (ms^.model)