{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module FeyState.Config (
    GraphicsLib(..), Config,
    width, height, far, near, aaSamples, hideCursor, fullScreen,
    windowTitle, dataPath, libraryPath, graphicsLib,
    loadConfig
) where

import Control.Lens (makeLenses)

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Number

data GraphicsLib = OpenGL | Vulkan

data Config = Config {
    _width :: Int,
    _height :: Int,
    _far :: Float,
    _near :: Float,
    _aaSamples :: Int,
    _hideCursor :: Bool,
    _fullScreen :: Bool,
    _windowTitle :: String,
    _dataPath :: String,
    _libraryPath :: String,
    _graphicsLib :: GraphicsLib
}

makeLenses ''Config

parseConfigFile :: Parser Config
parseConfigFile = do
    let numHelper = spaces >> nat
    let floatHelper = spaces >> floating
    let stringHelper = spaces >> many (noneOf "\r\n")

    w <- numHelper
    h <- numHelper
    f <- floatHelper
    n <- floatHelper
    aa <- numHelper
    cursor <- toEnum <$> numHelper
    full <- toEnum <$> numHelper
    title <- stringHelper
    dPath <- stringHelper
    lPath <- stringHelper
    gLib <- stringHelper >>= \ case
        "g" -> return OpenGL
        _ -> return Vulkan

    return $ Config w h f n aa cursor full title dPath lPath gLib

loadConfig :: FilePath -> IO Config
loadConfig path = do
    let fromEither (Right a) = a
    fromEither <$> parseFromFile parseConfigFile path