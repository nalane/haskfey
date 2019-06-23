{-# LANGUAGE TemplateHaskell #-}

{- |Contains functions for creating and interacting with models
-}

module Resources.Model (
    Model, texturePaths,
    createModel, drawModel, destroyModel
) where

import Graphics.Rendering.OpenGL
import Graphics

import System.IO

import Data.Either
import qualified Data.Vector as V
import Data.Vector ((!))

import Control.Lens (makeLenses, (^.))
import Control.Monad

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Number

data Material = Material (Vertex3 GLfloat) (Vertex3 GLfloat) Int

defaultMaterial :: Material
defaultMaterial = Material (Vertex3 0.8 0.8 0.8) (Vertex3 0.8 0.8 0.8) 1

-- |Contains the data for drawing a model to the screen
data Model = Model {
    _vao :: VertexArrayObject,
    _vbo :: BufferObject,
    _cbo :: BufferObject,
    _uvbo :: BufferObject,
    _buffLength :: Int,
    _texturePaths :: [String]
}

makeLenses ''Model

nVertParser :: Int -> Parser [GLfloat]
nVertParser n = mapM (\_ -> do
    spaces
    ap sign floating) [1..n]

vertex2Parser :: Parser (Vertex2 GLfloat)
vertex2Parser = do
    (x:y:_) <- nVertParser 2
    return $ Vertex2 x y

vertex3Parser :: Parser (Vertex3 GLfloat)
vertex3Parser = do
    (x:y:z:_) <- nVertParser 3
    return $ Vertex3 x y z

materialParser :: Parser Material
materialParser = do
    diffuse <- vertex3Parser
    specular <- vertex3Parser
    spaces
    Material diffuse specular <$> nat

textureParser :: Parser String
textureParser = many $ noneOf "\r\n"

uvParser :: Parser (Vertex2 GLfloat)
uvParser = do
    index <- nat
    spaces
    vertex2Parser

mapParser :: Parser (Int, Int)
mapParser = do
    vertIndex <- nat
    spaces
    uvIndex <- nat
    return (vertIndex, uvIndex)

helper :: Parser a -> Parser [a]
helper p = do
    num <- nat
    spaces
    count num (do
        res <- p
        spaces
        return res)

parser :: Parser (
    [Material], 
    V.Vector (Vertex3 GLfloat),
    [String],
    V.Vector (Vertex2 GLfloat),
    [(Int, Int)])
parser = do
    m <- helper materialParser
    v <- V.fromList <$> helper vertex3Parser
    t <- helper textureParser
    u <- V.fromList <$> helper uvParser
    a <- helper mapParser
    return (m, v, t, u, a)


-- |Read the model at the given path into memory.
createModel :: FilePath -> IO Model
createModel path = do
    let fromEither (Right a) = a
    let diffuse (Material d _ _) = d
    
    (mats, verts, texts, uvs, mapping) <- fromEither <$> parseFromFile parser path

    let mat =
            case mats of
                [] -> defaultMaterial
                _ -> head mats
    let v = map ((!) verts . fst) mapping
    let u = map ((!) uvs . snd) mapping
    let m = replicate (length v) $ diffuse mat
 
    vertexArray <- createVertexArray
    vertexBuffer <- createBuffer 0 3 v
    colorBuffer <- createBuffer 1 3 m
    uvBuffer <- createBuffer 2 2 u

    return $ Model vertexArray vertexBuffer colorBuffer uvBuffer (length v) texts

-- |Draw the given model to the screen
drawModel :: Model -> IO ()
drawModel m = do
    activateVertexArray (m^.vao)
    drawTriangles $ toEnum (m^.buffLength)

-- |Remove the given model from memory
destroyModel :: Model -> IO ()
destroyModel m = do
    deleteObjectNames [m^.vbo, m^.cbo]
    deleteObjectName (m^.vao)