{-# LANGUAGE TemplateHaskell #-}

module SpaceInvaders
    ( Game (..)
    , Monsters
    , Monster (..)
    , Spaceship (..)
    , GameKey (..)
    , handleActionKeys
    , gameInitialState
    , update
    ) where


-- *********************** Game domain ****************************

-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)
-- | Spaceship type
newtype Spaceship = Spaceship Position
-- | Monster type
newtype Monster = Monster Position
-- | Monster type alias
type  Monsters = [Monster]
-- | Game possible keys
data GameKey = ResetKey
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
update _ game = moveSpaceship game

-- | Move spaceship.
moveSpaceship
  :: Game -- ^ Game state to update
  -> Game -- ^ Game updated
moveSpaceship game = game
-- we need to move what in the game?


-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: Maybe GameKey -- ^ maybe a gameKey
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys (Just ResetKey) _ = gameInitialState
handleActionKeys Nothing game = game
-- Hint: pattern-match on the GameKey type
