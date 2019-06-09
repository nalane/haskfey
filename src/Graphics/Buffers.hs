{-| Contains functions for creating and manipulating OpenGL buffers
-}

module Graphics.Buffers (
    createBuffer, createVertexArray, activateVertexArray,
    getUniformLocation, setUniformMatrix,
    deleteObjectName, deleteObjectNames,
    VertexArrayObject(..), BufferObject(..), UniformLocation(..)
) where

import Matrix

import Graphics.Rendering.OpenGL

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- |Generates an OpenGL vertex buffer object
createBuffer :: Storable a => Int -> Int -> [a] -> IO BufferObject
createBuffer location stride vals = do
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
createVertexArray :: IO VertexArrayObject
createVertexArray = do
    vao <- genObjectName
    activateVertexArray vao
    return vao

-- |Sets the given vao as the active one
activateVertexArray :: VertexArrayObject -> IO ()
activateVertexArray vao = bindVertexArrayObject $= Just vao

-- |Gets the addressable location of the given variable in the program
getUniformLocation :: Program -> String -> IO UniformLocation
getUniformLocation prog var = get $ uniformLocation prog var

-- |Moves a 4x4 FeyMatrix to the specified ubo
setUniformMatrix :: UniformLocation -> FeyMatrix -> IO ()
setUniformMatrix ubo mat = do
    m <- newMatrix RowMajor $ concat mat :: IO (GLmatrix GLfloat)
    uniform ubo $= m