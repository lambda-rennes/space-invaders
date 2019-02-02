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

-- | Elapsed time since last cycle (in seconds)
type ElapsedTime = Float

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


-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , monsters = [Monster (0, 250)]
  }


-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
-- Game playing
update seconds game = moveSpaceship game

-- | Move spaceship.
moveSpaceship
  :: Game -- ^ Game state to update
  -> Game -- ^ Game updated
moveSpaceship game = game
-- we need to move what in the game?
