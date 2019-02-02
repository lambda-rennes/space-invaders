module SpaceInvaders
    ( Game
    , ImageLibrary(..)
    , mkInitialState
    , renderGame
    , handleKeys
    , update
      -- Re-exports.
    , module Window
    ) where

import Control.Lens

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Window

-- *********************** Game state ****************************

-- | Game record
newtype Game = Game
  { spaceship :: Position
  , monsters :: [Position]
  }

-- | Image library record
data ImageLibrary = ImageLibrary
  { _backgroundImg :: Gloss.Picture
  , _spaceshipImg :: Gloss.Picture
  , _monsterImg :: Gloss.Picture
  }

-- | spaceship
newtype Spaceship = Spaceship
  { pos :: (Float, Float)
  }



makeLenses ''ImageLibrary -- ^ needed to access easily to the record attr



-- | Create the initial game state from an image library.
mkInitialState
  :: ImageLibrary -- ^ Image library
  -> Game    -- ^ Initial game state
mkInitialState l = Game
  { monsters = [(0, 250)]
  , spaceship = (0, -250)
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: Float -- ^ Time passed since last update (in seconds)
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
-- Game playing
update seconds game = moveSpaceship game

-- | Move spaceship.
moveSpaceship
  :: Game -- ^ Game state to update
  -> Game -- ^ Game updated
moveSpaceship game = game
-- Hint: use record update syntax.

-- *********************** Key handling ************************

-- | Modify 'Game' state based on key events.
handleKeys
  :: Gloss.Event -- ^ keyEvent
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleKeys _ game = game
-- Hint: pattern-match on event key parameter (see Gloss documentation).

-- *********************** Rendering *****************************

-- | Render the 'Game' into a displayable 'Gloss.Picture'.
renderGame
  :: ImageLibrary -- ^ ImageLibrary
  -> Game -- ^ The game state to render
  -> Gloss.Picture -- ^ A picture of this game state
renderGame imgLib game = Gloss.pictures
  [ renderBackground imgLib
  , renderSpaceship (view spaceshipImg imgLib) (spaceship game)
  , renderMonsters (view monsterImg imgLib) (monsters game)
  ]

-- | Render the background image.
renderBackground
  :: ImageLibrary -- ^ Image library
  -> Gloss.Picture -- ^ Background picture
renderBackground library = view backgroundImg library

-- | Render the spaceship.
renderSpaceship
  :: Gloss.Picture -- ^ Spaceship Image
  -> (Float, Float) -- ^ Current spaceship (x,y) position
  -> Gloss.Picture -- ^ Picture of the spaceship
renderSpaceship spaceshipImg (x, y) =
  -- The picture of the spaceship is the corresponding library sprite translated
  -- by the spaceship coordinates.
  Gloss.translate x y $ spaceshipImg

-- | Render a monster
renderMonster
  :: Gloss.Picture -- ^ Monster image
  -> (Float, Float) -- ^ Monster (x,y) position
  -> Gloss.Picture -- ^ Picture of the monster
renderMonster monsterImg (x, y) =
  -- The picture of the monster is the corresponding library sprite translated
  -- by the spaceship coordinates.
  Gloss.translate x y $ monsterImg

-- ***************** TODO (Suggestions only) ******************

-- | Render multiple monsters in one go.
renderMonsters
  :: Gloss.Picture -- ^ Monster image
  -> [Position] -- ^ Monster positions.
  -> Gloss.Picture -- ^ Collage picture with all monsters represented.
renderMonsters mstImg monsters = Gloss.pictures (fmap (renderMonster mstImg) monsters)
-- Hint: think fmap, think currying !

-- | Render score
renderScores
  :: Int
  -> Gloss.Picture
renderScores = undefined
-- Hint : Use 'renderText' from Gloss.
