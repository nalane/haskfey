{-# LANGUAGE TemplateHaskell #-}

module Objects.CubeDude (
    CubeDude, load, update, draw, unload
) where

import Lib
import Data.IORef
import Data.StateVar
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL

data CubeDude = CubeDude {
    _prog :: Resource Program,
    _model :: Resource Model,
    _texture :: Resource Texture,
    _ubo :: UniformLocation,
    _time :: IORef Float
}

makeLenses ''CubeDude

instance Object CubeDude where
    load = do
        shd <- loadShader [
            (FragmentShader, "feyData/shaders/bare/bare.frag"),
            (VertexShader, "feyData/shaders/bare/bare.vert")]
        liftIO $ setShader $ unwrap shd

        mod <- loadModel "feyData/library/cube.fey.model"
        tex <- loadTexture ("feyData/library/" ++ head (unwrap mod ^. texturePaths))

        uni <- liftIO $ getUniformLocation (unwrap shd) "mvpMatrix"
        texLocation <- liftIO $ getUniformLocation (unwrap shd) "texSampler"
        GL.uniform texLocation $= GL.TextureUnit 1

        tim <- liftIO $ newIORef 0

        return $ CubeDude shd mod tex uni tim

    update s = (s^.time) $~ (+0.01)

    draw s cam = do
        liftIO $ setShader $ unwrap (s^.prog)

        t <- get (s^.time)
        liftIO $ setUniformMatrix (s^.ubo) $
            multiply cam $
            multiply (rotate t [1, 1, 1]) $
            scale 0.25
        
        liftIO $ drawTexture $ unwrap (s^.texture)
        liftIO $ drawModel $ unwrap (s^.model)

    unload s = do
        unloadTexture (s^.texture)
        unloadModel (s^.model)
        unloadShader (s^.prog)