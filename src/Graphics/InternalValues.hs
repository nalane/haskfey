{-| This structure contains the internal config of the graphics engine.
-}

{-# LANGUAGE TemplateHaskell #-}

module Graphics.InternalValues (
    InternalValues(..), vkInstance
) where

import Control.Lens (makeLenses)
import Graphics.Rendering.OpenGL as GL
import Vulkan as VK

-- |Persistent state of the engine
data InternalValues = OpenGL | Vulkan {
    _vkInstance :: VK.Instance
}

makeLenses ''InternalValues