{-# LANGUAGE TemplateHaskell #-}

module Components.Resources (
    feedData, readData,
    VertexResource, defaultVertexResource,
    MaterialResource, defaultMaterialResource,
    TextureResource, defaultTextureResource,
    UVResource, defaultUVResource
) where

import {-# SOURCE #-} FeyState.FeyState
import Matrix

import Data.IORef
import Control.Monad.IO.Class
import Control.Lens
import Data.Default
import Graphics
import Graphics.Rendering.OpenGL

data ResourceComponent a = ResourceComponent {
    _feedData :: a -> FeyState(),
    _readData :: FeyState a
}
makeLenses ''ResourceComponent

defaultResourceComponent :: Default a => FeyState (ResourceComponent a)
defaultResourceComponent = do
    container <- liftIO $ newIORef def
    return $ ResourceComponent {
        _feedData = liftIO . writeIORef container,
        _readData = liftIO $ readIORef container
    }

type VertexResource = ResourceComponent [Vertex3 GLfloat]
defaultVertexResource :: FeyState VertexResource
defaultVertexResource = defaultResourceComponent

type UVResource = ResourceComponent [Vertex2 GLfloat]
defaultUVResource :: FeyState UVResource
defaultUVResource = defaultResourceComponent

type MaterialResource = ResourceComponent Material
defaultMaterialResource :: FeyState MaterialResource
defaultMaterialResource = defaultResourceComponent

type TextureResource = ResourceComponent Texture
defaultTextureResource :: FeyState TextureResource
defaultTextureResource = defaultResourceComponent