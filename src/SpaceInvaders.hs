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

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Window

-- *********************** Game state ****************************

-- | Game type
newtype Game = Game
  { library :: ImageLibrary
  }

-- | Image library
data ImageLibrary = ImageLibrary
  { backgroundImg :: Gloss.Picture
  , spaceshipImg :: Gloss.Picture
  , monster1Img :: Gloss.Picture
  }

-- | spaceship
newtype Spaceship = Spaceship
  { pos :: (Float, Float)
  }

-- | Create the initial game state from an image library.
mkInitialState
  :: ImageLibrary -- ^ Image library
  -> Game    -- ^ Initial game state
mkInitialState l = Game
  { library = l -- Set the game image library as the argument.
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
  :: Game -- ^ The game state to render
  -> Gloss.Picture -- ^ A picture of this game state
renderGame game = Gloss.pictures
  [ renderBackground (library game)
  , renderSpaceship (library game) (0, -250)
  , renderMonster (library game) (0, 250)
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
  -> [(Float, Float)] -- ^ Monster positions.
  -> Gloss.Picture -- ^ Collage picture with all monsters represented.
renderMonsters = undefined
-- Hint: think fmap, think currying !

-- | Render score
renderScores
  :: Int
  -> Gloss.Picture
renderScores = undefined
-- Hint : Use 'renderText' from Gloss.
