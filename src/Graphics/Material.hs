{-# LANGUAGE TemplateHaskell #-}
module Graphics.Material (
    Material(..), diffuseMat, specularMat, specularIntensity,
    def
) where

import Graphics.Rendering.OpenGL
import Control.Lens
import Data.Default

data Material = Material {
    _diffuseMat :: Vertex3 GLfloat,
    _specularMat :: Vertex3 GLfloat,
    _specularIntensity :: Int
}
makeLenses ''Material

instance Default Material where
    def = Material (Vertex3 0.8 0.8 0.8) (Vertex3 0.8 0.8 0.8) 1