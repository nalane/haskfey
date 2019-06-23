module Object (
    Object(..)
) where

import FeyState
import Matrix

class Object a where
    load :: FeyState a
    update :: a -> FeyState ()
    draw :: a -> FeyMatrix -> FeyState ()
    unload :: a -> FeyState ()