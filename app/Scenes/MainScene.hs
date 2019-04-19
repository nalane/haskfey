{-# LANGUAGE TemplateHaskell #-}

module Scenes.MainScene (
    MainScene(..), initScene
) where

import Lib

import Graphics.UI.GLFW as GLFW

import Data.Maybe
import Data.Word

import Control.Monad
import Control.Lens (makeLenses, (^.))
import Control.Concurrent.MVar

data MainScene = MainScene {
    _prog :: Program,
    _vao :: VertexArrayObject,
    _vbo :: BufferObject,
    _cbo :: BufferObject,
    _ubo :: UniformLocation,
    _time :: MVar Float,
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

    timeVar <- execute $ newMVar 0
    let ms = MainScene shaderProg vertexArray vertexBuffer colorBuffer location timeVar cam proj
    return $ Scene (updateMainScene ms) (drawMainScene ms) (endMainScene ms)

updateMainScene :: MainScene -> FeyState (Maybe String)
updateMainScene ms = do
    win <- getStateVar window
    shouldEnd <- execute $ GLFW.getKey (fromJust win) GLFW.Key'Escape
    execute $ when (shouldEnd == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose (fromJust win) True

    execute $ modifyMVar_ (ms^.time) (return . (+0.01))

    swapScenes <- execute $ GLFW.getKey (fromJust win) GLFW.Key'Space
    if swapScenes == GLFW.KeyState'Pressed
        then return $ Just "second" 
        else return Nothing

drawMainScene :: MainScene -> FeyState ()
drawMainScene ms = do
    setShader (ms^.prog)

    activateVertexArray (ms^.vao)
    t <- execute $ readMVar (ms^.time)
    setUniformMatrix (ms^.ubo) $
        multiply (ms^.proj) $
        multiply (ms^.cam) $
        rotate t [0, 0, 1]
        
    drawIndexedTriangles indices

endMainScene :: MainScene -> FeyState ()
endMainScene ms = do
    unloadShader [
        (FragmentShader, "shaders/standard.frag"),
        (VertexShader, "shaders/standard.vert")]

    execute $ deleteObjectNames [ms^.vbo, ms^.cbo]
    execute $ deleteObjectName (ms^.vao)