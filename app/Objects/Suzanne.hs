{-# LANGUAGE TemplateHaskell #-}

module Objects.Suzanne (
    Suzanne, load, update, draw, unload
) where

import Lib
import Data.IORef
import Data.StateVar
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL

data Suzanne = Suzanne {
    _prog :: Resource Program,
    _model :: Resource Model,
    _texture :: Resource Texture,
    _ubo :: UniformLocation,
    _time :: IORef Float
}

makeLenses ''Suzanne

instance Object Suzanne where
    load = do
        shd <- loadShader [
            (FragmentShader, "feyData/shaders/bare/bare.frag"),
            (VertexShader, "feyData/shaders/bare/bare.vert")]
        liftIO $ setShader $ unwrap shd

        mod <- loadModel "feyData/library/monkey.fey.model"
        tex <- loadTexture ("feyData/library/" ++ head (unwrap mod ^. texturePaths))

        uni <- liftIO $ getUniformLocation (unwrap shd) "mvpMatrix"
        texLocation <- liftIO $ getUniformLocation (unwrap shd) "texSampler"
        GL.uniform texLocation $= GL.TextureUnit 0

        tim <- liftIO $ newIORef 0

        return $ Suzanne shd mod tex uni tim

    update s = (s^.time) $~ (+0.01)

    draw s cam = do
        liftIO $ setShader $ unwrap (s^.prog)

        t <- get (s^.time)
        liftIO $ setUniformMatrix (s^.ubo) $
            multiply cam $
            multiply (rotate t [0, 1, 0]) $
            multiply (rotate (-90) [1, 0, 0]) $
            scale 0.25
        
        liftIO $ drawTexture $ unwrap (s^.texture)
        liftIO $ drawModel $ unwrap (s^.model)

    unload s = do
        unloadTexture (s^.texture)
        unloadModel (s^.model)
        unloadShader (s^.prog)