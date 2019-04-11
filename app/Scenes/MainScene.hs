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
    let ms = MainScene shaderProg vertexArray vertexBuffer colorBuffer location timeVar cam proj
    return $ Scene (updateMainScene ms) (drawMainScene ms) (endMainScene ms)

updateMainScene :: MainScene -> State -> IO (Maybe String)
updateMainScene ms state = do
    let win = fromJust (state^.window)
    shouldEnd <- GLFW.getKey win GLFW.Key'Escape
    when (shouldEnd == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose win True

    modifyMVar_ (ms^.time) (return . (+0.01))

    swapScenes <- GLFW.getKey win GLFW.Key'Space
    if swapScenes == GLFW.KeyState'Pressed
        then return $ Just "second" 
        else return Nothing

drawMainScene :: MainScene -> State -> IO ()
drawMainScene ms _ = do
    setShader (ms^.prog)

    activateVertexArray (ms^.vao)
    withMVar (ms^.time) $ \t -> 
        setUniformMatrix (ms^.ubo) $
        multiply (ms^.proj) $
        multiply (ms^.cam) $
        rotate t [0, 0, 1]
        
    drawIndexedTriangles indices

endMainScene :: MainScene -> State -> IO ()
endMainScene ms state = do
    unloadShader state [
        (FragmentShader, "shaders/standard.frag"),
        (VertexShader, "shaders/standard.vert")]

    deleteObjectNames [ms^.vbo, ms^.cbo]
    deleteObjectName (ms^.vao)