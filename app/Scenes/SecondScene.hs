{-# LANGUAGE TemplateHaskell #-}

module Scenes.SecondScene (
    SecondScene, initScene
) where

import Lib

import Graphics.UI.GLFW as GLFW

import Data.Maybe
import Data.IORef
import Data.StateVar

import Control.Monad
import Control.Lens

import Objects.CubeDude

data SecondScene = SecondScene {
    _cam :: Camera,
    _cube :: CubeDude
}

makeLenses ''SecondScene

initScene :: FeyState Scene
initScene = do
    cam <- newCamera [0, 0, 0.5] [0, 0, 0] [0, 1, 0]
    c <- load

    let ms = SecondScene cam c
    return $ Scene (updateSecondScene ms) (drawSecondScene ms) (endSecondScene ms)

updateSecondScene :: SecondScene -> FeyState (Maybe String)
updateSecondScene ms = do
    win <- fromJust <$> getStateVar window
    shouldEnd <- isKeyPressed GLFW.Key'Escape
    when shouldEnd $ liftIO $ GLFW.setWindowShouldClose win True

    update (ms^.cube)

    swapScenes <- isKeyPressed GLFW.Key'Space
    if swapScenes
        then return $ Just "main" 
        else return Nothing

drawSecondScene :: SecondScene -> FeyState ()
drawSecondScene ms = draw (ms^.cube) $ getCameraMatrix (ms^.cam) 

endSecondScene :: SecondScene -> FeyState ()
endSecondScene ms = unload (ms^.cube)