module FeyState.ComponentDatabase (

) where

import Components

import Data.Map as M

newtype ResourceComponents = ResourceComponents {
    _vertexLists :: M.Map Int VertexList
}