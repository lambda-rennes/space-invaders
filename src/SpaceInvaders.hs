module SpaceInvaders
    ( Game (..)
    , Invaders
    , Invader (..)
    , Spaceship (..)
    , GameKey (..)
    , Laser (..)
    , handleActionKeys
    , gameInitialState
    , update
    , moveInvader
    ) where

-- *********************** Game domain ****************************

-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)
-- | Spaceship type
data Spaceship = Spaceship Position
-- | Invader type
data Invader = Invader Position
-- | Invader type alias
type  Invaders = [Invader]
-- | Game possible keys
data GameKey = ResetKey | LeftKey | RightKey | PiouPiou
-- | Game record
data Laser = Laser Position
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  , lasers :: [Laser]
  }

-- map :: (a -> b) -> [a] -> [b]

generateInvaderList
  :: Float -> Float -> Float -> [Invader]
--generateInvaderList minX maxX padding = map (\x -> Invader (x * padding, 250)) [minX..maxX]
generateInvaderList minX maxX padding 
  = [Invader (x * padding, y) | x <- [minX .. maxX], y <- [250, 190, 130]] 

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , invaders = generateInvaderList (-4) 4 100
  , lasers = []
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update _ game@Game {invaders = invs, lasers = lsr} = game {invaders = updateInvaders, lasers = updateLasers}
  where
    updateInvaders = moveInvaders (filterInvaders invs)
    updateLasers = moveLasers (filterLasers lsr)
    filterInvaders :: [Invader] -> [Invader]
    filterInvaders invs' = filter (\inv -> not (any (\l -> collision inv l) lsr)) invs'
    filterLasers lsrs' = filter (\l -> not (any (\inv -> collision inv l) invs)) lsrs'


    -- TODO need to move invaders too...

moveInvader
  :: Invader
  -> Invader
moveInvader (Invader (x, y)) = Invader (x, y - 0.5)

moveLaser
  :: Laser
  -> Laser
moveLaser (Laser (x, y)) = Laser (x, y + 4)

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> Maybe a -> Maybe b
-- fmap :: (a -> b) -> [a] -> [b]

moveInvaders
  :: [Invader]
  -> [Invader]
moveInvaders list = fmap moveInvader list

moveLasers
  :: [Laser]
  -> [Laser]
moveLasers list = fmap moveLaser list

collision
  :: Invader
  -> Laser
  -> Bool
collision (Invader (xi, yi)) (Laser (xl, yl)) = (abs(xi - xl) < 40 && abs(yi - yl) < 25)

-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: GameKey -- ^ gameKey to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys ResetKey _ = gameInitialState
handleActionKeys LeftKey game = 
  game { spaceship = moveSpaceshipLeft (spaceship game) }
handleActionKeys RightKey game =
  game { spaceship = moveSpaceshipRight (spaceship game) }
handleActionKeys PiouPiou game =
  game { lasers = createLaser (spaceship game) : (lasers game)}

moveSpaceshipBy :: Position -> Spaceship -> Spaceship
moveSpaceshipBy (x, y) (Spaceship (x', y')) = Spaceship (x+x', y+y')

moveSpaceshipLeft :: Spaceship -> Spaceship
moveSpaceshipLeft = moveSpaceshipBy (50, 0)

moveSpaceshipRight :: Spaceship -> Spaceship
moveSpaceshipRight = moveSpaceshipBy (-50, 0)

createLaser :: Spaceship -> Laser
createLaser (Spaceship (x, y)) = Laser (x, y)

-- TODO if you need an other game key
-- Hint: pattern-match on the GameKey type


-- ***************** TODO (Suggestions only) ******************
