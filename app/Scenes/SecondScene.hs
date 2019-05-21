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
    _prog :: Program,
    _vao :: VertexArrayObject,
    _vbo :: BufferObject,
    _cbo :: BufferObject,
    _ubo :: UniformLocation,
    _time :: IORef Float,
    _cam :: FeyMatrix,
    _proj :: FeyMatrix
}

makeLenses ''SecondScene

vertices :: [Vertex3 GLfloat]
vertices = [
    Vertex3 (-0.5) 0.5 0,
    Vertex3 (-0.5) (-0.5) 0,
    Vertex3 0.5 (-0.5) 0]

colors :: [Vertex3 GLfloat]
colors = [
    Vertex3 1 1 0,
    Vertex3 0 1 1,
    Vertex3 1 0 1]

initScene :: FeyState Scene
initScene = do
    shaderProg <- loadShader [
        (FragmentShader, "shaders/standard.frag"),
        (VertexShader, "shaders/standard.vert")]
    setShader shaderProg

    vertexArray <- createVertexArray
    vertexBuffer <- createBuffer 0 3 vertices
    colorBuffer <- createBuffer 1 3 colors

    w <- getStateVar width
    h <- getStateVar height
    let cam = camera [0, 0, 0.5] [0, 0, 0] [0, 1, 0]
    let proj = orthographic (fromJust w) (fromJust h)
    location <- getUniformLocation shaderProg "mvpMatrix"
    setUniformMatrix location $ multiply proj cam

    timeVar <- liftIO $ newIORef 0
    let ms = SecondScene {
        _prog = shaderProg,
        _vao = vertexArray,
        _vbo = vertexBuffer,
        _cbo = colorBuffer,
        _ubo = location,
        _time = timeVar,
        _cam = cam,
        _proj = proj }
    return $ Scene (updateSecondScene ms) (drawSecondScene ms) (endSecondScene ms)

updateSecondScene :: SecondScene -> FeyState (Maybe String)
updateSecondScene ms = do
    win <- getStateVar window
    shouldEnd <- liftIO $ GLFW.getKey (fromJust win) GLFW.Key'Escape
    liftIO $ when (shouldEnd == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose (fromJust win) True

    (ms^.time) $~ (+0.01)

    swapScenes <- liftIO $ GLFW.getKey (fromJust win) GLFW.Key'Space
    if swapScenes == GLFW.KeyState'Pressed
        then return $ Just "main" 
        else return Nothing

drawSecondScene :: SecondScene -> FeyState ()
drawSecondScene ms = do
    setShader (ms^.prog)

    activateVertexArray (ms^.vao)
    t <- get (ms^.time)
    setUniformMatrix (ms^.ubo) $
        multiply (ms^.proj) $
        multiply (ms^.cam) $
        rotate t [0, 1, 0]
        
    drawTriangles 3

endSecondScene :: SecondScene -> FeyState ()
endSecondScene ms = do
    unloadShader [
        (FragmentShader, "shaders/standard.frag"),
        (VertexShader, "shaders/standard.vert")]

    liftIO $ deleteObjectNames [ms^.vbo, ms^.cbo]
    liftIO $ deleteObjectName (ms^.vao)