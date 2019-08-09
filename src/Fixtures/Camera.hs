module Fixtures.Camera (
    Camera, newCamera, getCameraMatrix
) where

import Matrix
import FeyState

data Camera = Camera {
    _projection :: FeyMatrix,
    _view :: FeyMatrix
}

newCamera :: Vec3 -> Vec3 -> Vec3 -> FeyState Camera
newCamera pos target up = do
    w <- getStateVar (config.width)
    h <- getStateVar (config.height)

    let proj = orthographic w h
    let cam = camera pos target up

    return $ Camera proj cam

getCameraMatrix :: Camera -> FeyMatrix
getCameraMatrix (Camera p v) = multiply p v