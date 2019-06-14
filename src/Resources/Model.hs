{-# LANGUAGE TemplateHaskell #-}

module Resources.Model (
    Model, createModel, drawModel, destroyModel
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

data Model = Model {
    _vao :: VertexArrayObject,
    _vbo :: BufferObject,
    _cbo :: BufferObject,
    _buffLength :: Int
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
    V.Vector Material, 
    V.Vector (Vertex3 GLfloat),
    V.Vector String,
    V.Vector (Vertex2 GLfloat),
    V.Vector (Int, Int))
parser = do
    m <- V.fromList <$> helper materialParser
    v <- V.fromList <$> helper vertex3Parser
    t <- V.fromList <$> helper textureParser
    u <- V.fromList <$> helper uvParser
    a <- V.fromList <$> helper mapParser
    return (m, v, t, u, a)



createModel :: FilePath -> IO Model
createModel path = do
    let fromEither (Right a) = a
    let diffuse (Material d _ _) = d
    
    (mats, verts, _, _, mapping) <- fromEither <$> parseFromFile parser path

    let v = V.map (\(index,_) -> verts ! index) mapping
    let m = replicate (length v) (diffuse $ V.head $ V.snoc mats defaultMaterial)
 
    vertexArray <- createVertexArray
    vertexBuffer <- createBuffer 0 3 $ V.toList v
    colorBuffer <- createBuffer 1 3 m

    return $ Model vertexArray vertexBuffer colorBuffer (length v)

drawModel :: Model -> IO ()
drawModel m = do
    activateVertexArray (m^.vao)
    drawTriangles $ toEnum (m^.buffLength)

destroyModel :: Model -> IO ()
destroyModel m = do
    deleteObjectNames [m^.vbo, m^.cbo]
    deleteObjectName (m^.vao)