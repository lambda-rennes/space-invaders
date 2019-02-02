{-# LANGUAGE TemplateHaskell #-}

module SpaceInvaders
    ( Game (..)
    , Position
    , mkInitialState
    , update
    ) where

-- *********************** Game state ****************************

-- | Game record
data Game = Game
  { spaceship :: Position
  , monsters :: [Position]
  }

-- | Position
type Position = (Float, Float)


-- | Create the initial game state from an image library.
mkInitialState
  :: Game    -- ^ Initial game state
mkInitialState = Game
  { spaceship = (0, -250)
  , monsters = [(0, 250)]
  }


-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: Float -- ^ Time passed since last update (in seconds)
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
-- Game playing
update seconds game = moveSpaceship game

-- | Move spaceship.
moveSpaceship
  :: Game -- ^ Game state to update
  -> Game -- ^ Game updated
moveSpaceship game = game
-- Hint: use record update syntax.
