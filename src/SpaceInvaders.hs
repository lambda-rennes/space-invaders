{-# LANGUAGE TemplateHaskell #-}

module SpaceInvaders
    ( Game (..)
    , Invaders
    , Invader (..)
    , Spaceship (..)
    , GameKey (..)
    , Missile (..)
    , handleActionKeys
    , gameInitialState
    , update
    , spaceship
    , invaders
    , missiles
    ) where

import Control.Lens

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
-- | Invader type missile
newtype Missile = Missile Position
-- | Invader type missile
type Missiles = [Missile]
-- | Game possible keys
data GameKey = ResetKey | MoveRightKey | MoveLeftKey | ShootKey
-- | Game record
data Game = Game
  { _spaceship :: Spaceship
  , _invaders :: Invaders
  , _missiles :: Missiles
  }

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { _spaceship = Spaceship (0, -250)
  , _invaders = [Invader (x*100, 250+(x*25)) | x<-[-4..4]]
  , _missiles = []
  }

makeLenses ''Game -- needed to access easily to the record attr

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update elapsedTime game = (over invaders moveInvaders) . (over missiles moveMissiles) $ game
  where
    moveInvaders :: Invaders -> Invaders
    moveInvaders invds = fmap moveInvader invds
   
    moveInvader :: Invader -> Invader
    moveInvader (Invader (x, y)) = Invader (x, y - 5 * elapsedTime)
    -- TODO need to move invaders too...


moveMissiles 
  :: Missiles 
  -> Missiles
moveMissiles miss = fmap moveMissile miss

moveMissile 
  :: Missile 
  -> Missile 
moveMissile (Missile (x,y)) = Missile (x,y+2)

-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: GameKey -- ^ gameKey to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys ResetKey _ = gameInitialState
handleActionKeys MoveRightKey game = over spaceship (moveSpaceshipBy 10) game
handleActionKeys MoveLeftKey game = over spaceship (moveSpaceshipBy (-10)) game
handleActionKeys ShootKey game = over missiles (createMissile (getSpaceshipPosition game)) game

getSpaceshipPosition
  :: Game
  -> Position
getSpaceshipPosition (Game (Spaceship pos) _ _) = pos
 
createMissile
 :: Position
 -> Missiles
 -> Missiles
createMissile (x, y) mssles = Missile (x,y) : mssles



moveSpaceshipBy
  :: Float 
  -> Spaceship
  -> Spaceship
moveSpaceshipBy speed (Spaceship (x, y)) = Spaceship (x + speed, y)


-- TODO if you need an other game key
-- Hint: pattern-match on the GameKey type


-- ***************** TODO (Suggestions only) ******************

-- | TODO Move spaceship.


-- Hint : implement the function to move spaceship (maybe you need to change the signature)

-- | TODO Move invader.


-- | TODO Move invaders.


-- Hint :  implement function to move a list of invaders with fmap
