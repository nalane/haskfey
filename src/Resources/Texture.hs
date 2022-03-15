module Resources.Texture (
    Texture, createTexture, drawTexture, destroyTexture
) where

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Codec.Picture

import Data.Default
import Data.Either
import Data.Vector.Storable
import Data.Word

newtype Texture = Texture GL.TextureObject

instance Default Texture where
    def = Texture $ GL.TextureObject maxBound

createTexture :: FilePath -> IO (Either String Texture)
createTexture path = do
    rawImg <- readImage path
    case rawImg of
        Left err -> return $ Left err
        Right rawImg -> do
            let img = convertRGBA8 rawImg

            --GL.activeTexture $= GL.TextureUnit 0
            texObj <- GL.genObjectName
            GL.textureBinding GL.Texture2D $= Just texObj

            let (Image w32 h32 d) = img
            let w = toEnum w32
            let h = toEnum h32
            unsafeWith d $ \ptr ->
                GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D w h) 0 $
                GL.PixelData GL.RGBA GL.UnsignedByte ptr

            GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
            GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.ClampToEdge)
            GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.ClampToEdge)

            return $ Right $ Texture texObj

drawTexture :: Texture -> IO ()
drawTexture (Texture texObj) = do
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just texObj

destroyTexture :: Texture -> IO ()
destroyTexture (Texture texObj) =
    GL.deleteObjectName texObj