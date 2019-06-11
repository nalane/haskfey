{- |Contains functions for setting up shader programs
-}

module Resources.Shaders (
    createShaderProgram, destroyShaderProgram,
    ShaderType(..), Program(..)
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW

import System.IO
import Control.Monad

loadShader :: ShaderType -> String -> IO Shader
loadShader sType path = do
    handle <- openFile path ReadMode
    source <- hGetContents handle
    shader <- createShader sType
    shaderSourceBS shader $= packUtf8 source
    compileShader shader
    hClose handle
    
    status <- get $ compileStatus shader
    unless status $ do
        log <- get $ shaderInfoLog shader
        hPutStrLn stderr log
    return shader

loadProg :: [Shader] -> IO Program
loadProg shaders = do
    prog <- createProgram
    mapM_ (attachShader prog) shaders
    linkProgram prog

    status <- get $ linkStatus prog
    unless status $ do
        log <- get $ programInfoLog prog
        hPutStrLn stderr log

    mapM_ (detachShader prog) shaders
    return prog

-- |Given a list of tuples containing a shader type and path to a shader,
-- compiles the shaders and links them together.
createShaderProgram :: [(ShaderType, FilePath)] -> IO Program
createShaderProgram list = do
    shaders <- mapM (uncurry loadShader) list
    prog <- loadProg shaders
    deleteObjectNames shaders

    return prog

-- |Removes the given program from memory
destroyShaderProgram :: Program -> IO ()
destroyShaderProgram = deleteObjectName