{-| Contains functions for creating and manipulating OpenGL buffers
-}

module Graphics.Buffers (
    createBuffer, createVertexArray, activateVertexArray,
    getUniformLocation, setUniformMatrix,
    deleteObjectName, deleteObjectNames,
    VertexArrayObject(..), BufferObject(..), UniformLocation(..)
) where

import Matrix
import State

import Graphics.Rendering.OpenGL

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- |Generates an OpenGL vertex buffer object
createBuffer :: Storable a => Int -> Int -> [a] -> FeyState BufferObject
createBuffer location stride vals = liftIO $ do
    buffer <- genObjectName
    bindBuffer ArrayBuffer $= Just buffer

    withArray vals $ \ptr -> do
        let size = fromIntegral $ length vals * sizeOf (head vals)
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let attribLoc = AttribLocation $ fromIntegral location
    let descriptor = VertexArrayDescriptor (toEnum stride) Float 0 nullPtr
    vertexAttribPointer attribLoc $= (ToFloat, descriptor)
    vertexAttribArray attribLoc $= Enabled

    return buffer

-- |Generates a vertex array and sets it as the active one
createVertexArray :: FeyState VertexArrayObject
createVertexArray = do
    vao <- liftIO genObjectName
    activateVertexArray vao
    return vao

-- |Sets the given vao as the active one
activateVertexArray :: VertexArrayObject -> FeyState ()
activateVertexArray vao = liftIO (bindVertexArrayObject $= Just vao)

-- |Gets the addressable location of the given variable in the program
getUniformLocation :: Program -> String -> FeyState UniformLocation
getUniformLocation prog var = liftIO $ get $ uniformLocation prog var

-- |Moves a 4x4 FeyMatrix to the specified ubo
setUniformMatrix :: UniformLocation -> FeyMatrix -> FeyState ()
setUniformMatrix ubo mat = liftIO $ do
    m <- newMatrix RowMajor $ concat mat :: IO (GLmatrix GLfloat)
    uniform ubo $= m