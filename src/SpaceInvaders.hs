{-# LANGUAGE TemplateHaskell #-}

module SpaceInvaders
    ( Game (..)
    , Invaders
    , Invader (..)
    , Shots
    , Shot (..)
    , Spaceship (..)
    , GameKey (..)
    , collisionShotsInvaders
    , collisionShotsInvader
    , handleActionKeys
    , gameInitialState
    , update
    -- , moveInvader
    ) where

import Data.Maybe(catMaybes)
-- import Debug.Trace

-- *********************** Game domain ****************************
shotSpeed :: Float
spaceshipSpeed :: Float
invaderHeight :: Float
invaderWidth :: Float
windowMaxHeight :: Float

shotSpeed = 7
spaceshipSpeed = 10
invaderHeight = 49
invaderWidth = 49
windowMaxHeight = 320


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
newtype Invader = Invader Position deriving Show
-- | Shot type
newtype Shot = Shot Position deriving Show -- Shot (5,7)
-- | Shot type alias
type  Shots = [Shot]
-- | Invader type alias
type  Invaders = [Invader]
-- | spaceshipDirection Type
data SpaceshipDirection = Stop | MovingLeft | MovingRight
-- | Invaders' direction
data InvadersDirection = Tribord | Babord
-- | Game possible keys
data GameKey = ResetKey | LeftKeyUp | RightKeyUp | LeftKeyDown | RightKeyDown | SpaceKeyDown
-- | Game state
data GameState = Playing | Dead
-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  , spaceshipDirection :: SpaceshipDirection
  , shots :: Shots
  , invadersMovements :: (InvadersVector, TotalOffset, InvadersDirection)
  , gameState :: GameState
  }

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , invaders = createInvaders [(-430+x*130,y) | x <- [0..4], y <- [150,220,290]] 
  --, invaders = createInvaders [(0,0), (100,100)] 
  , spaceshipDirection = Stop
  , shots = []
  , invadersMovements = ((1,0), 0, Tribord)
  , gameState = Playing
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update _ game@Game {gameState = Dead} = game
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
        handleSpaceship game@Game {spaceshipDirection = MovingLeft} =
          game
            { spaceship = moveSpaceship (spaceship game) ((-1)*spaceshipSpeed) }
        handleSpaceship game@Game {spaceshipDirection = MovingRight} =
          game
            { spaceship = moveSpaceship (spaceship game) (spaceshipSpeed) }
        handleSpaceship game = game

        handleShots game =
          game { shots = deleteShots.moveShots $ shots game }

        handleInvadersShotsCollisions game =
          game{ invaders = i, shots = s }
            where (i, s) = collisionShotsInvaders (invaders game) (shots game)

--update _ game = game {invaders = moveInvaders (invaders game) (1,1) }
--update _ game@Game {spaceshipDirection = MovingLeft} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (-10)}
--update _ game@Game {spaceshipDirection = MovingRight} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (10)}
--update _ game@Game {spaceshipDirection = Stop} = game {shots = moveShots (shots game), spaceship = moveSpaceship (spaceship game) (0)}

-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: GameKey -- ^ gameKey to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys ResetKey _ = gameInitialState
handleActionKeys LeftKeyDown game@Game {spaceshipDirection = Stop} = game { spaceshipDirection = MovingLeft }
handleActionKeys RightKeyDown game@Game {spaceshipDirection = Stop} = game { spaceshipDirection = MovingRight }

handleActionKeys RightKeyDown game@Game {spaceshipDirection = MovingLeft} = game { spaceshipDirection = MovingRight }
handleActionKeys LeftKeyUp game@Game {spaceshipDirection = MovingLeft} = game { spaceshipDirection = Stop }

handleActionKeys LeftKeyDown game@Game {spaceshipDirection = MovingRight} = game { spaceshipDirection = MovingLeft }
handleActionKeys RightKeyUp game@Game {spaceshipDirection = MovingRight} = game { spaceshipDirection = Stop }

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
moveShots sshots = fmap (moveShot shotSpeed) sshots

moveShot 
  :: Float
  -> Shot
  -> Shot
moveShot offset (Shot (x,y)) = Shot (x, y+offset)

deleteShot
  :: Shot
  -> Maybe Shot
deleteShot (Shot (x,y)) = case (y > windowMaxHeight) of
                            True  -> Nothing
                            False -> Just $ Shot (x,y)

deleteShots
  :: Shots
  -> Shots
deleteShots sshots = catMaybes $ fmap (deleteShot) sshots

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
  -> Bool
collisionInvader (Invader (x', y')) (Shot (x, y)) =
  -- traceShowId $
  -- traceShow inv $
  -- traceShow shot $
  (x >= x' - invaderWidth) && (x <= x' + invaderWidth) && (y >= y' - invaderHeight) && (y <= y' + invaderHeight)

collisionShotsInvader
  :: Shots
  -> Invader
  -> Bool
collisionShotsInvader sshots invader = any (collisionInvader invader) sshots

collisionInvadersShot
  :: Invaders
  -> Shot
  -> Bool
collisionInvadersShot invaderss sshot = any ((flip collisionInvader) sshot) invaderss

collisionShotsInvaders
  :: Invaders
  -> Shots
  -> (Invaders, Shots)
collisionShotsInvaders invs sshots = (newInvs, newShots)
  where 
    newInvs = filter (\inv -> not $ collisionShotsInvader sshots inv) invs
    newShots = filter (\ss -> not $ collisionInvadersShot invs ss) sshots