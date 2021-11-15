{-# LANGUAGE TemplateHaskell #-}

module Components.Resources (
    feedData, readData,
    VertexResource, defaultVertexResource,
    MaterialResource, defaultMaterialResource,
    TextureResource, defaultTextureResource
) where

import {-# SOURCE #-} FeyState.FeyState
import Matrix

import Data.IORef
import Control.Monad.IO.Class
import Control.Lens
import Data.Default
import Graphics

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

type VertexResource = ResourceComponent [Vec3]
defaultVertexResource :: FeyState VertexResource
defaultVertexResource = defaultResourceComponent

type MaterialResource = ResourceComponent Material
defaultMaterialResource :: FeyState MaterialResource
defaultMaterialResource = defaultResourceComponent

type TextureResource = ResourceComponent Texture
defaultTextureResource :: FeyState TextureResource
defaultTextureResource = defaultResourceComponent