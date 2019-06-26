{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SpaceInvaders
    ( Game (..)
    , Invaders
    , Invader (..)
    , Shots
    , Shot (..)
    , Spaceship (..)
    , GameKey (..)
    , Score
    , GameState (..)
    , collisionShotsInvaders
    , collisionShotsInvader
    , handleActionKeys
    , gameInitialState
    , update
    -- , moveInvader
    ) where

import Data.Maybe(catMaybes)
import qualified System.Random as Random

-- import Debug.Trace

-- *********************** Game domain ****************************
shotSpeed :: Float
spaceshipSpeed :: Float
spaceshipHitboxSize :: Float
invaderHeight :: Float
invaderWidth :: Float
windowMaxHeight :: Float
invaderPoints :: Int
timeBetweenShots :: ElapsedTime
invincibilityTime :: ElapsedTime

shotSpeed = 7
spaceshipSpeed = 10
spaceshipHitboxSize = 25
invaderHeight = 49
invaderWidth = 49
windowMaxHeight = 320
invaderPoints = 100
timeBetweenShots = 1
invincibilityTime = 1


-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)
-- | Total offset applied to invaders since last direction change
type TotalOffset = Int
-- | Invader's vector
type InvadersVector = (Float, Float)
-- | Modulo to generate shot
type ModuloShot = Int
-- | Spaceship type
data Spaceship = Spaceship
  { position :: Position
  , direction :: SpaceshipDirection
  , timeSinceLastShot :: ElapsedTime
  , lives :: Lives
  , timeSinceLastHit :: ElapsedTime
  }
-- | Invader type
data Invader = Invader
  { moduloShot :: ModuloShot
  , positionInvader :: Position
  }
-- | Shot type
-- newtype Shot = Shot Position deriving Show -- Shot (5,7)
data Shot = InvaderShot Position | SpaceShipShot Position
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
data GameState = Playing | Dead | Win
-- | Score alias
type Score = Int
-- | Life counter
type Lives = Int
-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  , shots :: Shots
  , invadersMovements :: (InvadersVector, TotalOffset, InvadersDirection)
  , gameState :: GameState
  , score :: Score
  , randomGen :: Random.StdGen
  }

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship
    { position = (0, -250)
    , direction = Stop
    , timeSinceLastShot = timeBetweenShots
    , lives = 2
    , timeSinceLastHit = invincibilityTime
    }
  , invaders = createInvaders [1 .. 12]  [(-430+x*130,y) | x <- [0..4], y <- [150,220,290]]  --[3, 7, 2, 5, 9, 10, 7, 6, 3, 9, 6, 9 ]
  --, invaders = createInvaders [(0,0), (100,100)]
  , shots = []
  , invadersMovements = ((1,0), 0, Tribord)
  , gameState = Playing
  , score = 0
  , randomGen = Random.mkStdGen 666
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update _ game@Game {gameState = Dead} = game
update _ game@Game {gameState = Win} = game
update elapsedTime game' =
   (controlDeath .
   controlWin .
   handleInvadersShotsCollisions .
   handleUpdateInvadersVector .
   handleInvaders .
   handleSpaceshipMovements .
   handleShots .
   handleInvadersShots .
   handleShipCollision .
   handleUpdateSpaceshipTimers) game'
  where handleUpdateSpaceshipTimers game =
          game {spaceship = updateSpaceshipTimers (spaceship game) elapsedTime}
        handleInvaders game = game {invaders = moveInvaders (invaders game) a }
          where (a, _, _) = invadersMovements game
        handleUpdateInvadersVector game =
          game
            { invadersMovements = updateInvadersVector (invadersMovements game)}
        handleSpaceshipMovements game =
          game {spaceship = updateSpaceshipPosition (spaceship game)}
        handleShots game =
          game { shots = deleteShots . moveShots $ shots game }
        handleInvadersShotsCollisions game =
          game{ invaders = i, shots = s, score = newScore }
            where (i, s, shotInvs) = collisionShotsInvaders (invaders game) (shots game)
                  newScore = computeScore (score game) shotInvs
        handleInvadersShots game =
          game { shots = (shots game) ++ s, randomGen = r }
            where (s, r) = (updateInvadersShots (randomGen game) (invaders game))
        handleShipCollision game@Game { spaceship = Spaceship { timeSinceLastHit = t } }
          | t > invincibilityTime = game { spaceship = (spaceship game) { lives = newLives, timeSinceLastHit = newTimeSinceLastHit } }
          | otherwise = game
            where 
              isHit = collisionSpaceshipInvadersShots (shots game) (spaceship game)
              newLives = case isHit of
                          True -> (lives $ spaceship game) - 1
                          False -> (lives $ spaceship game)
              newTimeSinceLastHit = case isHit of
                                      True -> 0
                                      False -> t

updateSpaceshipPosition
  :: Spaceship
  -> Spaceship
updateSpaceshipPosition sp@Spaceship {direction = MovingLeft} =
  moveSpaceship sp ((-1)*spaceshipSpeed)
updateSpaceshipPosition sp@Spaceship {direction = MovingRight} =
  moveSpaceship sp spaceshipSpeed
updateSpaceshipPosition sp = sp

updateSpaceshipTimers
  :: Spaceship
  -> ElapsedTime
  -> Spaceship
updateSpaceshipTimers sp@Spaceship { timeSinceLastShot = t1, timeSinceLastHit = t2} elapsedTime =
  sp {timeSinceLastShot = newTimeSinceLastShot, timeSinceLastHit = newTimeSinceLastHit}
    where
      newTimeSinceLastShot = t1 + elapsedTime
      newTimeSinceLastHit = t2 + elapsedTime


-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: GameKey -- ^ gameKey to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys ResetKey _ = gameInitialState
handleActionKeys LeftKeyDown game@Game {spaceship = Spaceship{direction = Stop}} = game { spaceship = (spaceship game) {direction = MovingLeft} }
handleActionKeys RightKeyDown game@Game {spaceship = Spaceship{direction = Stop}} = game { spaceship = (spaceship game) {direction = MovingRight} }

handleActionKeys RightKeyDown game@Game {spaceship = Spaceship{direction = MovingLeft}} = game { spaceship = (spaceship game) {direction = MovingRight} }
handleActionKeys LeftKeyUp game@Game {spaceship = Spaceship{direction = MovingLeft}} = game {spaceship = (spaceship game) {direction = Stop}}

handleActionKeys LeftKeyDown game@Game {spaceship = Spaceship{direction = MovingRight}} = game { spaceship = (spaceship game) {direction = MovingLeft} }
handleActionKeys RightKeyUp game@Game {spaceship = Spaceship{direction = MovingRight}} = game {spaceship = (spaceship game) {direction = Stop}}

handleActionKeys SpaceKeyDown game = game {shots = (shots game) ++ newShot, spaceship = (spaceship game) { timeSinceLastShot = newT }}
  where (newShot, newT) = createSpaceshipShot (timeSinceLastShot (spaceship game)) (spaceship game)

handleActionKeys _ game = game

createSpaceshipShot
  :: ElapsedTime
  -> Spaceship
  -> ([Shot], ElapsedTime)
createSpaceshipShot t (Spaceship {position = (x, y)}) =
  case t > timeBetweenShots of
    True -> ([SpaceShipShot (x, y)], 0)
    _ -> ([], t)


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

moveSpaceship sp@Spaceship {position = (x, y)} step = sp { position = (x + step, y) }
-- Hint : implement the function to move spaceship (maybe you need to change the signature)

-- | TODO Move invader.
moveInvader
  :: Position
  -> Invader -- ^ initial invader
  -> Invader -- ^ invader updated
moveInvader (x', y') Invader{ moduloShot = m, positionInvader = (x, y)} = Invader m (x + x', y + y')

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
moveShot offset (SpaceShipShot (x,y)) = SpaceShipShot (x, y+offset)
moveShot offset (InvaderShot (x,y)) = InvaderShot (x, y-offset)

deleteShot
  :: Shot
  -> Maybe Shot
deleteShot (SpaceShipShot (x,y)) = case (y > windowMaxHeight) of
                            True  -> Nothing
                            False -> Just $ SpaceShipShot (x,y)

deleteShot (InvaderShot (x,y)) = case (y < - windowMaxHeight) of
                            True  -> Nothing
                            False -> Just $ InvaderShot (x,y)

deleteShots
  :: Shots
  -> Shots
deleteShots sshots = catMaybes $ fmap (deleteShot) sshots

createInvaders
  :: [ModuloShot]
  -> [Position]
  -> Invaders
createInvaders modulos positions = (<*>) (fmap Invader modulos) positions

collisionInvader
  :: Invader
  -> Shot
  -> Bool
collisionInvader Invader{positionInvader = (x', y')} (SpaceShipShot (x, y)) =
  -- traceShowId $
  -- traceShow inv $
  -- traceShow shot $
  (x >= x' - invaderWidth) && (x <= x' + invaderWidth) && (y >= y' - invaderHeight) && (y <= y' + invaderHeight)
collisionInvader _ _ = False

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
  -> (Invaders, Shots, Int) -- Int : number of touched invaders
collisionShotsInvaders invs sshots = (newInvs, newShots, touchedInvs)
  where
    newInvs = filter (\inv -> not $ collisionShotsInvader sshots inv) invs
    newShots = filter (\ss -> not $ collisionInvadersShot invs ss) sshots
    touchedInvs = (length invs) - (length newInvs)

collisionSpaceshipInvadersShots
  :: Shots
  -> Spaceship
  -> Bool
collisionSpaceshipInvadersShots sshots sspaceship = any (collisionSpaceshipShot sspaceship) sshots

collisionSpaceshipShot
  :: Spaceship
  -> Shot
  -> Bool
collisionSpaceshipShot _ (SpaceShipShot _ ) = False
collisionSpaceshipShot Spaceship {position = (x',y')} (InvaderShot (x,y)) = sqrt ((y'- y)**2 + (x' - x)**2) < spaceshipHitboxSize


controlDeath
  :: Game
  -> Game
controlDeath game = game { gameState = newGameState }
    where
      newGameState = case (bottomReachByInvaders (invaders game)) || (not $ enoughLives (lives $ spaceship game)) of
        False -> gameState game
        True -> Dead


bottomReachByInvaders
  :: Invaders
  -> Bool
bottomReachByInvaders = any (\Invader{positionInvader = (_, y)} -> y < -240 )

enoughLives
  :: Lives
  -> Bool
enoughLives l
  | l > 0 = True
  | otherwise = False

controlWin
  :: Game
  -> Game
controlWin game = case (invaders game) of
                      [] -> game { gameState = Win }
                      _  -> game

computeScore
  :: Score
  -> Int
  -> Score
computeScore currentScore shotInvs = currentScore + (shotInvs * invaderPoints)

updateInvadersShots
  :: Random.StdGen
  -> Invaders
  -> (Shots, Random.StdGen)
updateInvadersShots rand invs =
  case invs of
    [] -> ([], rand)
    (firstInv : othersInvs) -> (newList ++ newShot, otherRand)
      where
        (newList , otherRand) = updateInvadersShots newRand othersInvs
        (randomValue::Int, newRand) = Random.randomR (1, 10000) rand
        newShot = case randomValue of
          1 -> [ createInvaderShot firstInv ]
          _ -> []

createInvaderShot
  :: Invader
  -> Shot
createInvaderShot Invader{positionInvader = (x, y)} = InvaderShot (x, y)

