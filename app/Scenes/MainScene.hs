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
    _vao :: VertexArrayObject,
    _vbo :: BufferObject,
    _cbo :: BufferObject,
    _ubo :: UniformLocation,
    _time :: IORef Float,
    _cam :: FeyMatrix,
    _proj :: FeyMatrix
}

makeLenses ''MainScene

vertices :: [Vertex3 GLfloat]
vertices = [
    Vertex3 0.25 (-0.25) 0,
    Vertex3 0.25 0.25 0,
    Vertex3 (-0.25) (-0.25) 0,
    Vertex3 (-0.25) 0.25 0]

colors :: [Vertex3 GLfloat]
colors = [
    Vertex3 1 0 0,
    Vertex3 0 1 0,
    Vertex3 0 0 1,
    Vertex3 1 0 0]

indices :: [Word32]
indices = [0, 1, 2, 1, 3, 2]

initScene :: FeyState Scene
initScene = do
    shaderProg <- loadShader [
        (FragmentShader, "feyData/shaders/bare/bare.frag"),
        (VertexShader, "feyData/shaders/bare/bare.vert")]
    liftIO $ setShader $ unwrap shaderProg

    vertexArray <- liftIO createVertexArray
    vertexBuffer <- liftIO $ createBuffer 0 3 vertices
    colorBuffer <- liftIO $ createBuffer 1 3 colors

    w <- fromJust <$> getStateVar width
    h <- fromJust <$> getStateVar height
    let cam = camera [0, 0, 0.5] [0, 0, 0] [0, 1, 0]
    let proj = orthographic w h
    location <- liftIO $ getUniformLocation (unwrap shaderProg) "mvpMatrix"
    liftIO $ setUniformMatrix location $ multiply proj cam

    timeVar <- liftIO $ newIORef 0
    let ms = MainScene {
        _prog = shaderProg,
        _vao = vertexArray,
        _vbo = vertexBuffer,
        _cbo = colorBuffer,
        _ubo = location,
        _time = timeVar,
        _cam = cam,
        _proj = proj }
    return $ Scene (updateMainScene ms) (drawMainScene ms) (endMainScene ms)

updateMainScene :: MainScene -> FeyState (Maybe String)
updateMainScene ms = do
    win <- fromJust <$> getStateVar window
    shouldEnd <- liftIO $ GLFW.getKey win GLFW.Key'Escape
    liftIO $ when (shouldEnd == GLFW.KeyState'Pressed) $
        GLFW.setWindowShouldClose win True

    (ms^.time) $~ (+0.01)

    swapScenes <- liftIO $ GLFW.getKey win GLFW.Key'Space
    if swapScenes == GLFW.KeyState'Pressed
        then return $ Just "second" 
        else return Nothing

drawMainScene :: MainScene -> FeyState ()
drawMainScene ms = do
    liftIO $ setShader $ unwrap (ms^.prog)

    liftIO $ activateVertexArray (ms^.vao)
    t <- get (ms^.time)
    liftIO $ setUniformMatrix (ms^.ubo) $
        multiply (ms^.proj) $
        multiply (ms^.cam) $
        rotate t [0, 0, 1]
        
    liftIO $ drawIndexedTriangles indices

endMainScene :: MainScene -> FeyState ()
endMainScene ms = do
    unloadShader (ms^.prog)

    liftIO $ deleteObjectNames [ms^.vbo, ms^.cbo]
    liftIO $ deleteObjectName (ms^.vao)