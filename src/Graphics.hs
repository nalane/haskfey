{-| Module for interacting with graphics systems
-}

module Graphics (
    module Graphics.Initialization,
    module Graphics.Buffers,
    setShader, ShaderType(..), Program(..),
    drawTriangles, drawIndexedTriangles,
    Vertex3(..), GLfloat(..)
) where

import Graphics.Shader
import Graphics.Initialization
import Graphics.Buffers

import Graphics.Rendering.OpenGL
import Data.Word

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import State

-- |Draw the specified number of triangles
drawTriangles :: NumArrayIndices -> FeyState ()
drawTriangles = execute . drawArrays Triangles 0

-- |Draw the triangles using the indices specified by the input list
drawIndexedTriangles :: [Word32] -> FeyState ()
drawIndexedTriangles indices = execute $ withArray indices $ \ptr -> do
    let numIndices = toEnum $ length indices
    drawElements Triangles numIndices UnsignedInt ptr

-- |Set the currently active shader program
setShader :: Program -> FeyState ()
setShader prog = execute (currentProgram $= Just prog)