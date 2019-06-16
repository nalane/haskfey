{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module FeyState.Config (
    GraphicsLib(..), Config,
    width, height, aaSamples, hideCursor, fullScreen,
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
    let stringHelper = spaces >> many (noneOf "\r\n")

    w <- numHelper
    h <- numHelper
    aa <- numHelper
    cursor <- toEnum <$> numHelper :: Parser Bool
    full <- toEnum <$> numHelper :: Parser Bool
    title <- stringHelper
    dPath <- stringHelper
    lPath <- stringHelper
    gLib <- stringHelper >>= \ case
        "g" -> return OpenGL
        _ -> return Vulkan

    return $ Config w h aa cursor full title dPath lPath gLib

loadConfig :: FilePath -> IO Config
loadConfig path = do
    let fromEither (Right a) = a
    fromEither <$> parseFromFile parseConfigFile path