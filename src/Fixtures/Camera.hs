module Fixtures.Camera (
    Camera, newCamera, getCameraMatrix
) where

import Matrix
import FeyState
import Config

data Camera = Camera {
    _projection :: FeyMatrix,
    _view :: FeyMatrix
}

newCamera :: Vec3 -> Vec3 -> Vec3 -> FeyState Camera
newCamera pos target up = do
    w <- getStateVar (config.width)
    h <- getStateVar (config.height)
    f <- getStateVar (config.far)
    n <- getStateVar (config.near)

    let proj = orthographic w h f n
    let cam = camera pos target up

    return $ Camera proj cam

getCameraMatrix :: Camera -> FeyMatrix
getCameraMatrix (Camera p v) = multiply p v