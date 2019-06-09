{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Resources.Model (
    Model, createModel, drawModel, destroyModel
) where

import Graphics.Rendering.OpenGL
import Graphics

import System.IO
import Data.List
import Data.Either
import Control.Lens (makeLenses, (^.))
import Control.Monad

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Number
import Text.Parsec.Char

data Material = Material (Vertex3 GLfloat) (Vertex3 GLfloat) Int

_diffuse :: Material -> Vertex3 GLfloat
_diffuse (Material d _ _) = d

data Model = Model {
    _vao :: VertexArrayObject,
    _vbo :: BufferObject,
    _cbo :: BufferObject,
    _buffLength :: Int
}

makeLenses ''Model

--nVertParser :: Int -> Parsec s u [GLfloat]
nVertParser n = mapM (\_ -> do
    spaces
    ap sign floating) [1..n]

--vertex2Parser :: Parsec s u (Vertex2 GLfloat)
vertex2Parser = do
    (x:y:_) <- nVertParser 2
    return $ Vertex2 x y

--vertex3Parser :: Parsec s u (Vertex3 GLfloat)
vertex3Parser = do
    (x:y:z:_) <- nVertParser 3
    return $ Vertex3 x y z

--materialParser :: Parsec s u Material
materialParser = do
    diffuse <- vertex3Parser
    specular <- vertex3Parser
    spaces
    i <- read <$> many1 digit
    return $ Material diffuse specular i

--textureParser :: Parsec s u String
textureParser = many $ noneOf "\n"

--uvParser :: Parsec s u (Vertex2 GLfloat)
uvParser = do
    index <- many1 digit
    spaces
    vertex2Parser

--mapParser :: Parsec s u (Int, Int)
mapParser = do
    vertIndex <- read <$> many1 digit
    spaces
    uvIndex <- read <$> many1 digit
    return (vertIndex, uvIndex)

--helper :: Parsec s u a -> Parsec s u [a]
helper p = do
    num <- read <$> many1 digit
    spaces
    count num (do
        res <- p
        spaces
        return res)

parser :: Parser ([Material], [Vertex3 GLfloat], [String], [Vertex2 GLfloat], [(Int, Int)])
parser = do
    m <- helper materialParser
    v <- helper vertex3Parser
    t <- helper textureParser
    u <- helper uvParser
    a <- helper mapParser
    return (m, v, t, u, a)

{-
parseMats :: [String] -> ([Material], [String])
parseMats (num:dat) = (worker mats, rest) where
    n = read num :: Int
    (mats:rest) = splitAt (n * 7) dat

    diffuse (r:g:b:_) = Vertex3 (read r) (read g) (read b)
    specular (r:g:b:_) = Vertex3 (read r) (read g) (read b)
    mat (dr:dg:db:sr:sg:sb:i:_) =
        Material (diffuse [dr, dg, db]) (specular [sr, sg, sb]) $ read i

    worker [] = []
    worker arr = let (m:r) = splitAt 7 arr in
        mat m : worker r

parseVerts :: [String] -> ([Vertex3 GLfloat], [String])
parseVerts (num:dat) = (worker verts, rest) where
    n = read num :: Int
    (verts:rest) = splitAt (n * 3) dat
    worker [] = []
    worker (x:y:z:fin) = Vertex3 (read x) (read y) (read z) : worker fin

parseTextures :: [String] -> ([String], [String])
parseTextures (num:dat) = (texts, rest) where
    n = read num :: Int
    (texts:rest) = splitAt n dat

parseFeyFile :: [String] -> Model
parseFeyFile dat = t where
    (mats, matRest) = parseMats dat
    (verts, vertRest) = parseVerts matRest
    (texts, textRest) = parseTextures vertRest
-}

createModel :: FilePath -> IO Model
createModel path = do
    (mats, verts, texts, uvs, mapping) <- fromRight ([], [], [], [], []) <$>
        parseFromFile parser path

    let v = map (\(index,_) -> verts !! index) mapping
    let u = map (\(_, index) -> uvs !! index) mapping
    let m = replicate (length v) (_diffuse $ head mats)

    vertexArray <- createVertexArray
    vertexBuffer <- createBuffer 0 3 v
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