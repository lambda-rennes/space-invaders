{-# LANGUAGE TypeApplications #-}
module SpaceInvaders
    ( Game (..)
    , Invaders
    , Invader (..)
    , Spaceship (..)
    , GameKey (..)
    , Projectile (..)
    , Projectiles
    , handleActionKeys
    , gameInitialState
    , update
    , moveInvader
    ) where

import Window
-- *********************** Game domain ****************************

-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)
-- | Spaceship type
newtype Spaceship = Spaceship Position
-- | Invader type
newtype Invader = Invader Position
-- | Invader type alias
type  Invaders = [Invader]
newtype Projectile = Projectile Position
type Projectiles = [Projectile]
-- | Game possible keys
data GameKey = ResetKey | MoveLeft | MoveRight | StopMoving | Shoot
-- | Moving state
data MovingState = Immobile | MovingLeft | MovingRight
-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  , movingState :: MovingState
  , projectiles :: Projectiles
  }

initInvaders :: Invaders
initInvaders = 
  do 
    i <-[-2..2]
    j <- [0,1]
    return (Invader (fromIntegral @Int (i*84), fromIntegral @Int (150 + j*42)))  

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , invaders = initInvaders
  , movingState = Immobile
  , projectiles = []
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update _  = 
  gameFilterInvaders . gameMoveProjectiles . gameMoveInvaders . gameMoveSpaceship
  where
    gameMoveSpaceship (game@Game { spaceship = s, movingState = mvstate }) = 
      game { spaceship = moveSpaceship s mvstate }
    gameMoveInvaders (game@Game { invaders = is }) = 
      game { invaders = moveInvaders is }
    gameFilterInvaders (game@Game { invaders = is, projectiles = ps }) =
      game { invaders = filterInvaders is ps }
    gameMoveProjectiles (game@Game { projectiles = ps }) =
      game { projectiles = moveProjectiles ps } -- game.copy( projectiles = moveProjectiles(game.projectiles))
    -- TODO need to move invaders too...

filterInvaders :: Invaders -> Projectiles -> Invaders
filterInvaders invaders' projectiles' =
  filter (\inv -> not $ any (\proj -> detectCollision inv proj) projectiles') invaders'

detectCollision :: Invader -> Projectile -> Bool
detectCollision (Invader(ix,iy)) (Projectile(px,py)) =
  distance < 42**2
  where
    distance = (ix - px )**2 + (iy - py)**2


-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: GameKey -- ^ gameKey to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys ResetKey _ = gameInitialState
handleActionKeys StopMoving game =  game { movingState = Immobile }
handleActionKeys MoveLeft game = game {movingState = MovingLeft}
handleActionKeys MoveRight game = game {movingState = MovingRight}
handleActionKeys Shoot game@(Game (Spaceship (x,y)) _ _ prjctiles) =
   game {projectiles = Projectile(x, y + 42) : prjctiles}
-- TODO if you need an other game key
-- Hint: pattern-match on the GameKey type


-- ***************** TODO (Suggestions only) ******************

-- | TODO Move spaceship.
moveSpaceship
  :: Spaceship -- ^ initial Spaceship
  -> MovingState -- ^ moving state
  -> Spaceship -- ^ Spaceship updated
moveSpaceship ship Immobile = ship -- identity - we do nothing
moveSpaceship (Spaceship (x,y)) MovingRight = Spaceship(fromIntegral $ limitSpaceshipPos(round (x+10) :: Int) , y)
moveSpaceship (Spaceship (x,y)) MovingLeft = Spaceship(fromIntegral $ limitSpaceshipPos(round (x-10) :: Int), y)
-- Hint : implement the function to move spaceship (maybe you need to change the signature)

moveProjectiles
  :: Projectiles
  -> Projectiles

moveProjectiles = fmap (\(Projectile(x,y)) -> Projectile(x, y + 1))

limitSpaceshipPos :: Int -> Int
limitSpaceshipPos x = 
  let w = fst winSize
      half = w `div` 2
  in ((x + half) `mod` w) - half

-- | TODO Move invader.
moveInvader
  :: Invader -- ^ initial invader
  -> Invader -- ^ invader updated
moveInvader (Invader (x,y)) = Invader (x, y-5) -- identity - we do nothing

-- | TODO Move invaders.
moveInvaders
  :: Invaders -- ^ initial list of invaders
  -> Invaders -- ^ list of invaders updated
moveInvaders = fmap moveInvader -- identity - we do nothing
-- Hint :  implement function to move a list of invaders with fmap
