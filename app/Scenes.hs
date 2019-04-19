module Scenes (
    sceneMapping
) where

import Lib
import Scenes.MainScene
import Scenes.SecondScene
import Data.Map

sceneMapping :: String -> FeyState Scene
sceneMapping = (!) $ fromList [
    ("main", Scenes.MainScene.initScene),
    ("second", Scenes.SecondScene.initScene)]