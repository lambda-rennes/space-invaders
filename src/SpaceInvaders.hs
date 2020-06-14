module SpaceInvaders
    ( Game (..)
    , Invaders
    ,Bullet (..)
    , Invader (..)
    , Spaceship (..)
    , GameAction (..)
    , handleAction
    , gameInitialState
    , update
    , moveInvader
    ) where

import Data.Ix

-- *********************** Game domain ****************************

-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)

type Offset = (Float, Float)
-- | Spaceship type
newtype Spaceship = Spaceship Position
-- | Invader type
newtype Bullet = Bullet Position

newtype Invader = Invader Position
-- | Invader type alias
type  Invaders = [Invader]
-- | Game possible actions
data GameAction
  = ResetAction
  | LeftAction
  | RightAction
  | ShootAction
-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  , bullets :: [Bullet]
  }
spaceshipWidth , spaceshipHeight :: Float
(spaceshipWidth, spaceshipHeight) = (95, 80)

inBound :: Position -> Bool
inBound (x,y) = abs  x <= 960 / 2 && abs y <= 640 / 2

invaderWidth, invaderHeight :: Float
(invaderWidth, invaderHeight) = (95, 80)

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , invaders = createInvaders 10 3
  , bullets = []
  }
createInvaders :: Int -> Int -> Invaders
createInvaders cols rows =
  [ Invader (fromIntegral x * invaderWidth - 960/2 + invaderWidth/2, 250 - fromIntegral y * invaderHeight) | x <- [0..cols-1], y <- [0..rows-1]]
-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update _ game@Game {spaceship = sp, invaders = invs, bullets = buls} = game {invaders = updatedInvaders, bullets = updatedBullets}
  where
    updatedInvaders = moveInvaders invs
    updatedBullets = moveBullets buls
    -- TODO need to move invaders and spaceship...


-- | Modify 'Game' state based on GameActions.
handleAction
  :: GameAction -- ^ gameAction to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleAction ResetAction _ = gameInitialState
handleAction RightAction gs = gs { spaceship = moveSpaceship (spaceship gs) 10 }
handleAction LeftAction  gs = gs { spaceship = moveSpaceship (spaceship gs) (-10) }
handleAction ShootAction gs = gs { bullets = newBullet : bullets gs }
  where (Spaceship (x, y)) = spaceship gs
        newBullet = Bullet (x, y + spaceshipHeight/2 + 20)

-- TODO if you need an other game action
-- Hint: pattern-match on the GameAction type


-- ***************** TODO (Suggestions only) ******************

-- | TODO Move spaceship.
moveSpaceship :: Spaceship -> Float -> Spaceship 
-- ^ Spaceship updated
moveSpaceship (Spaceship (x,y)) dx = Spaceship( x + dx, y)
-- Hint : implement the function to move spaceship (maybe you need to change the signature)

moveInvader
  :: Invader --initial invader
  -> Invader -- ^ invader updated
--moveInvader  (first param) (second param) (third)
moveInvader (Invader p@(x, y)) = -- Invader (x + 5, y)
    Invader $ if inBound p' then p' else (x, y + dy) -- what if you hit the bottom? mod?
  where
    dx = 1
    dy = -81 -- need to be odd number
    movingRight = odd (round y :: Int)
    x' = if movingRight then x + dx else x - dx
    p' = (x',y)
-- moveInvader oldInvader = newInvader
-- deconstruct here?
-- moveInvader (Invader (x, y)) =

-- | TODO Move invaders.
moveInvaders
  :: Invaders -- ^ initial list of invaders
  -> Invaders -- ^ list of invaders updated
  --fmap :: Functor f => (a -> b) -> f a -> f b
  -- map :: (a -> b) -> [a] -> [b] 
moveInvaders = fmap moveInvader -- identity - we do nothing
-- Hint :  implement function to move a list of invaders with fmap
-- fmap moveInvader _ _ invaders??? -blackeuler
-- ^ seems right

moveBullets :: [Bullet] -> [Bullet]
moveBullets = fmap (\(Bullet (x,y)) -> Bullet (x, y + dy))
  where dy = 10