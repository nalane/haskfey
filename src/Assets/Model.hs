module Assets.Model (

) where

import Control.Monad
import Control.Lens ((^.), un)

import Data.Vector ((!))
import qualified Data.Vector as V

import Graphics.Rendering.OpenGL
import Graphics

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Number
import Text.ParserCombinators.Parsec.Error

import Components
import FeyState

nVertParser :: Int -> Parser [GLfloat]
nVertParser n = replicateM n (spaces *> sign <*> floating)

vertex2Parser :: Parser (Vertex2 GLfloat)
vertex2Parser = do
    (x:y:_) <- nVertParser 2
    return $ Vertex2 x y

vertex3Parser :: Parser (Vertex3 GLfloat)
vertex3Parser = do
    (x:y:z:_) <- nVertParser 3
    return $ Vertex3 x y z

materialParser :: Parser Material
materialParser = Material 
    <$> vertex3Parser
    <*> vertex3Parser 
    <*  spaces 
    <*> nat

textureParser :: Parser String
textureParser = many $ noneOf "\r\n"

uvParser :: Parser (Vertex2 GLfloat)
uvParser = nat *> space *> vertex2Parser

mapParser :: Parser (Int, Int)
mapParser = (,)
    <$> nat
    <*  spaces
    <*> nat

helper :: Parser a -> Parser [a]
helper p = do
    num <- nat <* spaces
    count num (p <* spaces)

parser :: Parser (
    [Material], 
    V.Vector (Vertex3 GLfloat),
    [String],
    V.Vector (Vertex2 GLfloat),
    [(Int, Int)])
parser = (,,,,)
    <$> helper materialParser
    <*> (V.fromList <$> helper vertex3Parser)
    <*> helper textureParser
    <*> (V.fromList <$> helper uvParser)
    <*> helper mapParser

loadModel :: 
    FilePath -> 
    VertexResource ->
    MaterialResource -> 
    TextureResource -> 
    UVResource -> 
    FeyState (Either String ())
loadModel path vr mr tr ur = do
    eitherVal <- liftIO $ parseFromFile parser path
    case eitherVal of
        Left e -> return $ Left $ unlines $ map messageString $ errorMessages e
        Right (mats, verts, texts, uvs, mapping) -> do 
            let mat = if null mats then def else head mats
            let v = map ((!) verts . fst) mapping
            let u = map ((!) uvs . snd) mapping
            let m = replicate (length v) $ mat ^. diffuseMat
 
            (vr ^. feedData) v
            (mr ^. feedData) mat
            --(tr ^. feedData) texts
            (ur ^. feedData) u
            return $ Right ()