{-# LANGUAGE TemplateHaskell #-}
module FeyState.ComponentDatabase (
    ComponentDatabase, resourceComponents,
    vertexLists
) where

import Components

import Data.Map as M
import Control.Lens ( makeLenses )
import Data.Default

newtype ResourceComponents = ResourceComponents {
    _vertexLists :: M.Map Int VertexList
}
makeLenses ''ResourceComponents

instance Default ResourceComponents where
    def = ResourceComponents def



newtype ComponentDatabase = ComponentDatabase {
    _resourceComponents :: ResourceComponents
}
makeLenses ''ComponentDatabase

instance Default ComponentDatabase where
    def = ComponentDatabase def