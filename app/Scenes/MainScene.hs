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

import Objects.Suzanne

data MainScene = MainScene {
    _cam :: FeyMatrix,
    _proj :: FeyMatrix,
    _suz :: Suzanne
}

makeLenses ''MainScene

initScene :: FeyState Scene
initScene = do
    w <- getStateVar (config.width)
    h <- getStateVar (config.height)
    let cam = camera [0, 0, 0.5] [0, 0, 0] [0, 1, 0]
    let proj = orthographic w h

    s <- load
    let ms = MainScene {
        _cam = cam,
        _proj = proj,
        _suz = s }
    return $ Scene (updateMainScene ms) (drawMainScene ms) (endMainScene ms)

updateMainScene :: MainScene -> FeyState (Maybe String)
updateMainScene ms = do
    win <- fromJust <$> getStateVar window
    shouldEnd <- isKeyPressed GLFW.Key'Escape
    when shouldEnd $ liftIO $ GLFW.setWindowShouldClose win True

    update (ms^.suz)

    swapScenes <- isKeyPressed GLFW.Key'Space
    if swapScenes
        then return $ Just "second" 
        else return Nothing

drawMainScene :: MainScene -> FeyState ()
drawMainScene ms = draw (ms^.suz) $ multiply (ms^.proj) (ms^.cam) 

endMainScene :: MainScene -> FeyState ()
endMainScene ms = unload (ms^.suz)