{- |Contains the functions for using the FeyState monad
-}

module FeyState.FeyState (
    FeyState(..), fmap, pure, (<*>), (>>), (>>=), return, liftIO,
    getStateVar, setStateVar,
    runFeyState
) where

import FeyState.State

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

-- |The FeyState monad itself
newtype FeyState a = FeyState (State -> IO (State, a))

instance Functor FeyState where
    -- |Given a function and a monadic value,
    -- |take the value and apply it to the function
    fmap f (FeyState s) = FeyState (\state -> do
        (newState, val) <- s state
        return (newState, f val))

instance Applicative FeyState where
    pure val = FeyState (\state -> return (state, val))
    (<*>) (FeyState lhs) (FeyState rhs) = FeyState (\state -> do
        (newState, f) <- lhs state
        (finalState, val) <- rhs newState
        return (finalState, f val))

instance Monad FeyState where
    (>>) (FeyState lhs) (FeyState rhs) = FeyState (\state -> do
        (newState, _) <- lhs state
        rhs newState)

    (>>=) (FeyState lhs) f = FeyState (\state -> do
        (newState, val) <- lhs state
        let (FeyState rhs) = f val
        rhs newState)

    return = pure

instance MonadIO FeyState where
    liftIO action = FeyState (\state -> do
        val <- action
        return (state, val))

-- |Returns the value referred to by the Lens
getStateVar :: Getting a State a -> FeyState a
getStateVar getter = FeyState (\state -> return (state, state^.getter))

-- |Set the state value referred to by the given Lens
setStateVar :: ASetter State State a b -> b -> FeyState ()
setStateVar setter val = FeyState (\state -> return (set setter val state, ()))

-- |Runs the FeyState and pushes it into the IO monad
runFeyState :: String -> FeyState a -> IO a
runFeyState path (FeyState f) = do
    state <- newState path
    (_, res) <- f state
    return res