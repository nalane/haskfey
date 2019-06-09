{-| Module for interacting with graphics systems
-}

module Graphics (
    module Graphics.Initialization,
    module Graphics.Buffers,
    drawTriangles, drawIndexedTriangles, setShader,
    Vertex3(..), GLfloat(..)
) where

import Graphics.Initialization
import Graphics.Buffers

import Graphics.Rendering.OpenGL
import Data.Word

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- |Draw the specified number of triangles
drawTriangles :: NumArrayIndices -> IO ()
drawTriangles = drawArrays Triangles 0

-- |Draw the triangles using the indices specified by the input list
drawIndexedTriangles :: [Word32] -> IO ()
drawIndexedTriangles indices = withArray indices $ \ptr -> do
    let numIndices = toEnum $ length indices
    drawElements Triangles numIndices UnsignedInt ptr

-- |Set the currently active shader program
setShader :: Program -> IO ()
setShader prog = currentProgram $= Just prog