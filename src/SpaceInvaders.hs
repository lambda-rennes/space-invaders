{-# LANGUAGE TemplateHaskell #-}

module SpaceInvaders
    ( Game (..)
    , Invaders
    , Invader (..)
    , Shots
    , Shot (..)
    , Spaceship (..)
    , GameKey (..)
    , handleActionKeys
    , gameInitialState
    , update
    -- , moveInvader
    ) where



-- *********************** Game domain ****************************

-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)
-- | Spaceship type
newtype Spaceship = Spaceship Position
-- | Invader type
newtype Invader = Invader Position
-- | Shot type
newtype Shot = Shot Position -- Shot (5,7)
-- | Shot type alias
type  Shots = [Shot]
-- | Invader type alias
type  Invaders = [Invader]
-- | Etat Type
data Etat = Stop | MovingLeft | MovingRight 
-- | Game possible keys
data GameKey = ResetKey | LeftKeyUp | RightKeyUp | LeftKeyDown | RightKeyDown | SpaceKeyDown
-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  , etat :: Etat
  , shots :: Shots
  }

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , invaders = [Invader (0, 250)]
  , etat = Stop
  , shots = []
  }


-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update _ game@Game {etat = MovingLeft} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (-10)}
update _ game@Game {etat = MovingRight} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (10)}
update _ game@Game {etat = Stop} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (0)}

-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: GameKey -- ^ gameKey to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys ResetKey _ = gameInitialState
handleActionKeys LeftKeyDown game@Game {etat = Stop} = game { etat = MovingLeft }
handleActionKeys RightKeyDown game@Game {etat = Stop} = game { etat = MovingRight }

handleActionKeys RightKeyDown game@Game {etat = MovingLeft} = game { etat = MovingRight }
handleActionKeys LeftKeyUp game@Game {etat = MovingLeft} = game { etat = Stop }

handleActionKeys LeftKeyDown game@Game {etat = MovingRight} = game { etat = MovingLeft }
handleActionKeys RightKeyUp game@Game {etat = MovingRight} = game { etat = Stop }

handleActionKeys SpaceKeyDown game = game {shots = (shots game) ++ [Shot (x, y)]}
  where Spaceship (x, y) = spaceship game

handleActionKeys _ game = game


-- TODO if you need an other game key
-- Hint: pattern-match on the GameKey type


-- ***************** TODO (Suggestions only) ******************

-- | TODO Move spaceship.
moveSpaceship
  :: Spaceship -- ^ initial Spaceship
  -> Float
  -> Spaceship -- ^ Spaceship updated

moveSpaceship (Spaceship (x, y)) step = Spaceship (x + step, y)
-- Hint : implement the function to move spaceship (maybe you need to change the signature)

-- -- | TODO Move invader.
-- moveInvader
--   :: Invader -- ^ initial invader
--   -> Invader -- ^ invader updated
-- moveInvader = id -- identity - we do nothing

-- -- | TODO Move invaders.
-- moveInvaders
--   :: Invaders -- ^ initial list of invaders
--   -> Invaders -- ^ list of invaders updated
-- moveInvaders = id -- identity - we do nothing
-- -- Hint :  implement function to move a list of invaders with fmap

moveShots
  :: Shots
  -> Shots
moveShots sshots = fmap (moveShot 10) sshots

moveShot 
  :: Float
  -> Shot
  -> Shot
moveShot offset (Shot (x,y)) = Shot (x, y+offset)