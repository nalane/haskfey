{-# LANGUAGE TemplateHaskell #-}
module FeyState.ComponentDatabase (
    ComponentDatabase, resourceComponents,
    vertexResources
) where

import Components

import Data.Map as M
import Control.Lens
import Data.Default

data ResourceComponents = ResourceComponents {
    _vertexResources :: M.Map Int VertexResource,
    _materialResources :: M.Map Int MaterialResource,
    _textureResource :: M.Map Int TextureResource
}
makeLenses ''ResourceComponents

instance Default ResourceComponents where
    def = ResourceComponents def def def



newtype ComponentDatabase = ComponentDatabase {
    _resourceComponents :: ResourceComponents
}
makeLenses ''ComponentDatabase

instance Default ComponentDatabase where
    def = ComponentDatabase def