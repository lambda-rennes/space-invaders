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

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Window

-- *********************** Game state ****************************

-- | Game type
data Game = Game
  { library :: ImageLibrary
  , spaceship :: Position
  , monsters :: [Position]
  , mDirection :: Direction
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
  [(i*100,j*100) | i <- [-3..3],
         j <- [0..3] ]

mkInitialState
  :: ImageLibrary -- ^ Image library
  -> Game    -- ^ Initial game state
mkInitialState l = Game
  { library = l -- Set the game image library as the argument.
  , spaceship = (0, -250)
  , monsters = generateMonstersPosition
  , mDirection = Down
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: Float -- ^ Time passed since last update (in seconds)
  -> Game -- ^ Current game state
  -> IO Game -- ^ Updated game state.
-- Game playing
update _ game = return $
  moveMonsters game

move
  :: Float
  -> Float
  -> Position
  -> Position
move dx dy (x, y) =
  (x + dx, y + dy)

-- | Move spaceship.
moveSpaceship
  :: Float
  -> Game -- ^ Game state to update
  -> Game -- ^ Game updated
moveSpaceship delta game = game {
    spaceship = move delta 0 (spaceship game)
  }

moveMonsters
  :: Game
  -> Game
moveMonsters game =
  game {
    mDirection = if getLowestPosition (monsters game) < -200 then Up else (mDirection game)
    , monsters = fmap (move 0 (directionFactor (mDirection game))) (monsters game)
  }

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

-- *********************** Key handling ************************

handleKeysIO
  :: Gloss.Event
  -> Game
  -> IO Game
handleKeysIO e g = return $
  handleKeys e g

-- | Modify 'Game' state based on key events.
handleKeys
  :: Gloss.Event -- ^ keyEvent
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleKeys (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) Gloss.Down _ _) game = moveSpaceship (-10) game
handleKeys (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) Gloss.Down _ _) game = moveSpaceship 10 game
handleKeys _ game = game

-- Hint: pattern-match on event key parameter (see Gloss documentation).

-- *********************** Rendering *****************************

-- | Render the 'Game' into a displayable 'Gloss.Picture'.
renderGame
  :: Game -- ^ The game state to render
  -> Gloss.Picture -- ^ A picture of this game state
renderGame game = Gloss.pictures
  [ renderBackground (library game)
  , renderSpaceship (library game) (spaceship game)
  , renderMonsters (library game) (monsters game)
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
  Gloss.translate x y $ spaceshipImg library

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
