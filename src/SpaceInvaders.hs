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

import Data.Maybe(catMaybes, listToMaybe)

-- *********************** Game domain ****************************

-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)
-- | Total offset applied to invaders since last direction change
type TotalOffset = Int
-- | Invader's vector
type InvadersVector = (Float, Float)
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
-- | Invaders' direction
data InvadersDirection = Tribord | Babord
-- | Game possible keys
data GameKey = ResetKey | LeftKeyUp | RightKeyUp | LeftKeyDown | RightKeyDown | SpaceKeyDown
-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  , etat :: Etat
  , shots :: Shots
  , invadersMovements :: (InvadersVector, TotalOffset, InvadersDirection)
  }

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , invaders = createInvaders [(-430+x*130,y) | x <- [0..4], y <- [150,220,290]] 
  , etat = Stop
  , shots = []
  , invadersMovements = ((1,0), 0, Tribord)
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.

update _ game' =
   ( handleInvadersShotsCollisions . 
   handleUpdateInvadersVector .
   handleInvaders .
   handleSpaceship .
   handleShots) game'
  where handleInvaders game = game {invaders = moveInvaders (invaders game) a }
          where (a, _, _) = invadersMovements game
        handleUpdateInvadersVector game =
          game
            { invadersMovements = updateInvadersVector (invadersMovements game)}
        handleSpaceship game@Game {etat = MovingLeft} =
          game
            { spaceship = moveSpaceship (spaceship game) (-10) }
        handleSpaceship game@Game {etat = MovingRight} =
          game
            { spaceship = moveSpaceship (spaceship game) (10) }
        handleSpaceship game = game

        handleShots game =
          game { shots = moveShots (shots game) }

        handleInvadersShotsCollisions game =
          game{ invaders = collisionShotsInvaders (invaders game) (shots game) }

--update _ game = game {invaders = moveInvaders (invaders game) (1,1) }
--update _ game@Game {etat = MovingLeft} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (-10)}
--update _ game@Game {etat = MovingRight} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (10)}
--update _ game@Game {etat = Stop} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (0)}

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

updateInvadersVector
  :: (InvadersVector, TotalOffset, InvadersDirection)
  -> (InvadersVector, TotalOffset, InvadersDirection)
updateInvadersVector (_, 200, invadersDirection) = ((0, (-80)), 0, inverseDirection invadersDirection)
  where inverseDirection :: InvadersDirection -> InvadersDirection
        inverseDirection Tribord = Babord
        inverseDirection Babord = Tribord
updateInvadersVector (_, totalOffset, Tribord) = ((1, 0), totalOffset + 1, Tribord) 
updateInvadersVector (_, totalOffset, Babord) = ((-1, 0), totalOffset + 1, Babord)

-- | TODO Move spaceship.
moveSpaceship
  :: Spaceship -- ^ initial Spaceship
  -> Float
  -> Spaceship -- ^ Spaceship updated

moveSpaceship (Spaceship (x, y)) step = Spaceship (x + step, y)
-- Hint : implement the function to move spaceship (maybe you need to change the signature)

-- | TODO Move invader.
moveInvader
  :: Position
  -> Invader -- ^ initial invader
  -> Invader -- ^ invader updated
moveInvader (x', y') (Invader(x,y)) = Invader (x + x', y + y') -- 

-- -- | TODO Move invaders.
moveInvaders
  :: Invaders -- ^ initial list of invaders
  -> Position
  -> Invaders -- ^ list of invaders updated
moveInvaders invs pos = fmap (moveInvader pos) invs -- identity - we do nothing
-- Hint :  implement function to move a list of invaders with fmap

moveShots
  :: Shots
  -> Shots
moveShots sshots = fmap (moveShot 10) sshots

moveShot 
  :: Float
  -> Shot
  -> Shot
moveShot offset (Shot (x,y)) = Shot (x, y+offset)

createInvaders
  :: [Position]
  -> Invaders
createInvaders positions = fmap (createInvader) positions

createInvader
 :: Position
 -> Invader
createInvader (x,y) = Invader (x,y)

collisionInvader
  :: Invader
  -> Shot
  -> Maybe Invader
collisionInvader inv@(Invader (x', y')) (Shot (x, y)) = 
  case (x' < x) && (x < x + 98) && (y' < y) && (y' < y + 98) of
    True  -> Nothing
    False -> Just inv

collisionShotsInvader
  :: Shots
  -> Invader
  -> Maybe Invader
collisionShotsInvader sshots invader = 
  case (length sshots) == 0 of
    True  -> Just invader
    False -> listToMaybe (catMaybes (fmap (collisionInvader invader) sshots))

collisionShotsInvaders
  :: Invaders
  -> Shots
  -> Invaders
collisionShotsInvaders invs sshots = catMaybes (fmap (collisionShotsInvader sshots) invs)