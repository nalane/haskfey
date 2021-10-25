{- |Contains functions for setting up shader programs
-}

module Resources.Shaders (
    createShaderProgram, destroyShaderProgram, setShader,
    ShaderType(..), Program
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW

import System.IO
import Control.Monad
import Data.ByteString as BS (readFile) 
import Data.List
import Data.Either

loadShader :: ShaderType -> String -> IO (Either String Shader)
loadShader sType path = do
    source <- BS.readFile path
    shader <- createShader sType
    shaderSourceBS shader $= source
    compileShader shader
    
    status <- get $ compileStatus shader
    if status then 
        return $ Right shader 
    else
        Left <$> get (shaderInfoLog shader)

loadProg :: [Shader] -> IO (Either String Program)
loadProg shaders = do
    prog <- createProgram
    mapM_ (attachShader prog) shaders
    linkProgram prog
    mapM_ (detachShader prog) shaders

    status <- get $ linkStatus prog
    if status then
        return $ Right prog
    else
        Left <$> get (programInfoLog prog)

-- |Given a list of tuples containing a shader type and path to a shader,
-- compiles the shaders and links them together.
createShaderProgram :: [(ShaderType, FilePath)] -> IO (Either String Program)
createShaderProgram list = do
    eitherShaders <- mapM (uncurry loadShader) list
    let errors = lefts eitherShaders
    if null errors then do
        let shaders = rights eitherShaders
        prog <- loadProg shaders
        deleteObjectNames shaders

        return prog
    else return $ Left $ head errors

-- |Removes the given program from memory
destroyShaderProgram :: Program -> IO ()
destroyShaderProgram = deleteObjectName

-- |Set the currently active shader program
setShader :: Program -> IO ()
setShader prog = currentProgram $= Just prog