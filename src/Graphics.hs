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

import FeyState

-- |Draw the specified number of triangles
drawTriangles :: NumArrayIndices -> FeyState ()
drawTriangles = liftIO . drawArrays Triangles 0

-- |Draw the triangles using the indices specified by the input list
drawIndexedTriangles :: [Word32] -> FeyState ()
drawIndexedTriangles indices = liftIO $ withArray indices $ \ptr -> do
    let numIndices = toEnum $ length indices
    drawElements Triangles numIndices UnsignedInt ptr

-- |Set the currently active shader program
setShader :: Program -> FeyState ()
setShader prog = currentProgram $= Just prog