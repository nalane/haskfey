{-# LANGUAGE TemplateHaskell #-}

module Components.Resources (
    feedData, readData,
    VertexList, defaultVertexList
) where

import {-# SOURCE #-} FeyState.FeyState
import Matrix

import Data.IORef
import Control.Monad.IO.Class
import Control.Lens

data ResourceComponent a = ResourceComponent {
    _feedData :: [a] -> FeyState(),
    _readData :: FeyState [a]
}
makeLenses ''ResourceComponent

defaultResourceComponent :: FeyState (ResourceComponent a)
defaultResourceComponent = do
    container <- liftIO $ newIORef []
    return $ ResourceComponent {
        _feedData = liftIO . writeIORef container,
        _readData = liftIO $ readIORef container
    }

type VertexList = ResourceComponent Vec3
defaultVertexList :: FeyState VertexList
defaultVertexList = defaultResourceComponent