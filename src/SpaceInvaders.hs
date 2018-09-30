module SpaceInvaders
    ( Game
    , ImageLibrary(..)
    , mkInitialState
    , renderGame
    , handleKeysIO
    , update
      -- Re-exports.
    , module Window
    ) where

import Connector
import Message
import Control.Concurrent.MVar
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Data.Map as Map
import Data.Map (Map)

import Window

-- *********************** Game state ****************************

-- XXX: Now that we use the IO interface, we should move to a 'ReaderT _ IO'
-- pattern for the various handlers and remove the game library and players map
-- from the game state.

-- | Game type
data Game = Game
  { library :: ImageLibrary
  , spaceship :: Position
  , spaceshipSpd :: Maybe Float
  , missiles :: [Position]
  , monsters :: [Position]
  , mDirection :: Direction
  , otherPlayers :: MVar (Map PlayerID XPosition)
  , connector :: Connector
  }

-- | Image library
data ImageLibrary = ImageLibrary
  { backgroundImg :: Gloss.Picture
  , spaceshipImg :: Gloss.Picture
  , monster1Img :: Gloss.Picture
  }

-- | position
type Position = (Float, Float)

data Direction = Up | Down

-- | Create the initial game state from an image library.
-- generateur de -300 à 300 en x et -300 à 0 en y de 100 en 100
generateMonstersPosition
  :: [Position]
generateMonstersPosition =
  [ (i*100,j*100)
  | i <- [-3..3],
    j <- [0..3] ]

mkInitialState
  :: ImageLibrary -- ^ Image library
  -> PlayerID
  -> IO Game    -- ^ Initial game state
mkInitialState l playerId = do
  playersMap <- newMVar $ Map.empty
  con <- startConnector playerId playersMap
  return $ Game
    { library = l -- Set the game image library as the argument.
    , spaceship = (0, -250)
    , spaceshipSpd = Nothing
    , missiles = []
    , monsters = generateMonstersPosition
    , mDirection = Down
    , otherPlayers = playersMap
    , connector = con
    }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: Float -- ^ Time passed since last update (in seconds)
  -> Game -- ^ Current game state
  -> IO Game -- ^ Updated game state.
-- Game playing
update _ game = do
  g <- sendPosition . moveMissiles . moveSpaceship . moveMonsters $ game
  addOthersMissiles g

move
  :: Float
  -> Float
  -> Position
  -> Position
move dx dy (x, y) =
  (x + dx, y + dy)

addOthersMissiles
  :: Game
  -> IO Game
addOthersMissiles game = do
  newMissiles <- (getNewMissiles $ connector game)
  let otherMissiles = fmap (\n -> (snd n, -250)) newMissiles
      newGame = game {missiles = otherMissiles ++ (missiles game)}
  return newGame

-- | send position to the network on change
sendPosition
  :: Game -- ^ Game state
  -> IO Game -- ^ IO Game si somthing goes wrong
sendPosition newGame@(Game {spaceshipSpd = Just _, spaceship = (spaceShipX, _)}) = do
   _ <- (sendMessage (connector newGame)) (NewPosition $ spaceShipX)
   return newGame
sendPosition game = return game

-- | Move spaceship.
moveSpaceship
  :: Game -- ^ Game state to update
  -> Game -- ^ Game updated
moveSpaceship game@(Game {spaceshipSpd = Nothing}) = game
moveSpaceship game@(Game {spaceshipSpd = Just spd, spaceship = (x, y)}) =
  game {spaceship = (newSpdX, y)}
  where
    newSpdX = spd + x

moveMonsters
  :: Game
  -> Game
moveMonsters game =
  game {
    mDirection = if getLowestPosition (monsters game) < -200 then Up else (mDirection game)
    , monsters = fmap (move 0 (directionFactor (mDirection game))) (monsters game)
  }

moveMissiles
  :: Game
  -> Game
moveMissiles game = game {missiles = newMissiles}
  where
    newMissiles = fmap (\(xpos, ypos) -> (xpos, ypos + 5::Float)) (missiles game)


directionFactor
  :: Direction
  -> Float
directionFactor direction = case direction of
  Up -> 1
  Down -> -1

getLowestPosition
  :: [Position]
  -> Float
getLowestPosition = foldr minPos 3000
  where minPos (_, y) minValue = min y minValue

-- Hint: use record update syntax.
--
-- ********************** Key handling ************************

handleKeysIO
  :: Gloss.Event
  -> Game
  -> IO Game
handleKeysIO (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) Gloss.Down _ _) game = do
  let newGame = game {missiles = (spaceShipX, spaceShipY):(missiles game)}
  _ <- (sendMessage (connector newGame)) (Missile $ spaceShipX)
  return newGame
  where
    (spaceShipX, spaceShipY) = (spaceship game)

handleKeysIO event game = return $ handleKeys event game

handleKeys
  :: Gloss.Event
  -> Game
  -> Game
handleKeys (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) Gloss.Down _ _) game =
  game {spaceshipSpd = Just (-10.0)}
handleKeys (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) Gloss.Up _ _) game =
  game {spaceshipSpd = Nothing}

handleKeys (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) Gloss.Down _ _) game =
  game {spaceshipSpd = Just 10.0}
handleKeys (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) Gloss.Up _ _) game =
  game {spaceshipSpd = Nothing}



handleKeys _ game = game

-- Hint: pattern-match on event key parameter (see Gloss documentation).

-- *********************** Rendering *****************************

-- | Render the 'Game' into a displayable 'Gloss.Picture'.
renderGame
  :: Game -- ^ The game state to render
  -> IO Gloss.Picture -- ^ A picture of this game state
renderGame game =  do
  pweet <- readMVar $ otherPlayers game
  return $ Gloss.pictures
    [ renderBackground (library game)
    , renderOtherPlayers (library game) (connectorsToPositions (Map.toList pweet))
    , renderSpaceship (library game) (spaceship game)
    , renderMonsters (library game) (monsters game)
    , renderMissiles (missiles game)
    ]

-- | Render the background image.
renderBackground
  :: ImageLibrary -- ^ Image library
  -> Gloss.Picture -- ^ Background picture
renderBackground library = backgroundImg library

-- | Render the spaceship.
renderSpaceship
  :: ImageLibrary -- ^ Image library
  -> (Float, Float) -- ^ Current spaceship (x,y) position
  -> Gloss.Picture -- ^ Picture of the spaceship
renderSpaceship library (x, y) =
  -- The picture of the spaceship is the corresponding library sprite translated
  -- by the spaceship coordinates.
  Gloss.pictures [spaceship, mark]
  where
    spaceship = Gloss.translate x y $ spaceshipImg library
    mark = Gloss.color Gloss.red $ Gloss.translate x y $ Gloss.circleSolid 10

-- | Render spaceship missile
renderMissile
  :: Position -- ^ missile position
  -> Gloss.Picture
renderMissile (x, y) =
  Gloss.color Gloss.blue
  $ Gloss.translate x y
  $ Gloss.rectangleSolid 5 5

-- | Render Missiles
renderMissiles
  :: [Position]
  -> Gloss.Picture
renderMissiles xs = Gloss.pictures $ fmap renderMissile xs

-- | Render a monster
renderMonster
  :: ImageLibrary -- ^ Image library
  -> (Float, Float) -- ^ Monster (x,y) position
  -> Gloss.Picture -- ^ Picture of the monster
renderMonster library (x, y) =
  -- The picture of the monster is the corresponding library sprite translated
  -- by the spaceship coordinates.
  Gloss.translate x y $ monster1Img library

-- ***************** TODO (Suggestions only) ******************

-- | Render multiple monsters in one go.
renderMonsters
  :: ImageLibrary
  -> [Position] -- ^ Monster positions.
  -> Gloss.Picture -- ^ Collage picture with all monsters represented.
renderMonsters imageLib monsters = Gloss.pictures (fmap (renderMonster imageLib) monsters)
-- Hint: think fmap, think currying !

-- | Render score
renderScores
  :: Int
  -> Gloss.Picture
renderScores = undefined
-- Hint : Use 'renderText' from Gloss.


connectorsToPositions
  :: [(PlayerID, XPosition)]
  -> [Position]
connectorsToPositions = fmap connectorToPosition

connectorToPosition
  :: (PlayerID, XPosition)
  -> Position
connectorToPosition (_, x) = (x, -250)

-- | Render other players spaceships
renderOtherPlayers
  :: ImageLibrary -- ^ Image library
  -> [Position]
  -> Gloss.Picture
renderOtherPlayers imageLib otherPlayers = Gloss.pictures (fmap (renderOtherPlayer imageLib) otherPlayers)

-- | Render other player spaceship
renderOtherPlayer
  :: ImageLibrary -- ^ Image library
  -> Position
  -> Gloss.Picture
renderOtherPlayer library (x, y) = Gloss.translate x y $ spaceshipImg library

-- ************************** Colision **************************************

missileMonsterCollision
  :: Position -- missile position
  -> Position -- monster position
  -> Maybe Position -- monster or not if killed
missileMonsterCollision (misX, misY) (monsX, monsY) =
  if (misY + 5) >= (monsY - 98)
    && (misX - 5) >= (monsX - 98)
    && (misX + 5) <= (monsX + 98) then
    Nothing
  else
    Just (monsX, monsY)
