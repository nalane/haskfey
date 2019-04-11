{-# LANGUAGE TemplateHaskell #-}

module Scenes.SecondScene (
    SecondScene(..), initScene
) where

import Lib

import Graphics.UI.GLFW as GLFW

import Data.Maybe

import Control.Monad
import Control.Lens
import Control.Concurrent.MVar

data SecondScene = SecondScene {
    _prog :: Program,
    _vao :: VertexArrayObject,
    _vbo :: BufferObject,
    _cbo :: BufferObject,
    _ubo :: UniformLocation,
    _time :: MVar Float,
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

initScene :: State -> IO Scene
initScene state = do
    shaderProg <- loadShader state [
        (FragmentShader, "shaders/standard.frag"),
        (VertexShader, "shaders/standard.vert")]
    setShader shaderProg

    vertexArray <- createVertexArray
    vertexBuffer <- createBuffer 0 3 vertices
    colorBuffer <- createBuffer 1 3 colors

    let w = fromJust (state^.width)
    let h = fromJust (state^.height)
    let cam = camera [0, 0, 0.5] [0, 0, 0] [0, 1, 0]
    let proj = orthographic w h
    location <- getUniformLocation shaderProg "mvpMatrix"
    setUniformMatrix location $ multiply proj cam

    timeVar <- newMVar 0
    let ms = SecondScene shaderProg vertexArray vertexBuffer colorBuffer location timeVar cam proj
    return $ Scene (updateSecondScene ms) (drawSecondScene ms) (endSecondScene ms)

updateSecondScene :: SecondScene -> State -> IO (Maybe String)
updateSecondScene ms state = do
    let win = fromJust (state^.window)
    shouldEnd <- GLFW.getKey win GLFW.Key'Escape
    when (shouldEnd == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose win True

    modifyMVar_ (ms^.time) (return . (+0.01))

    swapScenes <- GLFW.getKey win GLFW.Key'Space
    if swapScenes == GLFW.KeyState'Pressed
        then return $ Just "main" 
        else return Nothing

drawSecondScene :: SecondScene -> State -> IO ()
drawSecondScene ms _ = do
    setShader (ms^.prog)

    activateVertexArray (ms^.vao)
    withMVar (ms^.time) $ \t -> setUniformMatrix (ms^.ubo) $
        multiply (ms^.proj) $
        multiply (ms^.cam) $
        rotate t [0, 1, 0]
        
    drawTriangles 3

endSecondScene :: SecondScene -> State -> IO ()
endSecondScene ms state = do
    unloadShader state [
        (FragmentShader, "shaders/standard.frag"),
        (VertexShader, "shaders/standard.vert")]

    deleteObjectNames [ms^.vbo, ms^.cbo]
    deleteObjectName (ms^.vao)