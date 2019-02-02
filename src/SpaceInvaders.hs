{-# LANGUAGE TemplateHaskell #-}

module SpaceInvaders
    ( Game (..)
    , Monsters
    , Monster (..)
    , Spaceship (..)
    , gameInitialState
    , update
    ) where


import Control.Lens

-- *********************** Game domain ****************************


-- | Position type alias
type Position = (Float, Float)

-- | Spaceship type
newtype Spaceship = Spaceship Position

-- | Monster type
newtype Monster = Monster Position

-- | Monster type alias
type  Monsters = [Monster]

-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , monsters :: Monsters
  }


-- | Create the initial game state from an image library.
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , monsters = [Monster (0, 250)]
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
