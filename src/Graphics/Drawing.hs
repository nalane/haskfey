module Graphics.Drawing (
    drawTriangles, drawIndexedTriangles
) where

import Graphics.Rendering.OpenGL
import Data.Word

import Foreign.Marshal.Array

-- |Draw the specified number of triangles
drawTriangles :: NumArrayIndices -> IO ()
drawTriangles = drawArrays Triangles 0

-- |Draw the triangles using the indices specified by the input list
drawIndexedTriangles :: [Word32] -> IO ()
drawIndexedTriangles indices = withArray indices $ \ptr -> do
    let numIndices = toEnum $ length indices
    drawElements Triangles numIndices UnsignedInt ptr