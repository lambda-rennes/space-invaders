{-# LANGUAGE TemplateHaskell #-}

module GlossSpaceInvadersInterface
  ( ImageLibrary(..)
  , renderGame
  , handleKeys
  , update
    -- Re-exports.
  , module Window
  ) where

  import SpaceInvaders

  import Control.Lens

  import qualified Graphics.Gloss as Gloss
  import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
  import Window

  -- *************** Gloss SpaceInvaders Interface ****************

  -- | Image library record
  data ImageLibrary = ImageLibrary
    { _backgroundImg :: Gloss.Picture
    , _spaceshipImg :: Gloss.Picture
    , _monsterImg :: Gloss.Picture
    }

  makeLenses ''ImageLibrary -- ^ needed to access easily to the record attr


  -- *********************** Rendering *****************************

  renderGame
  -- | Render the 'Game' into a displayable 'Gloss.Picture'.
    :: ImageLibrary -- ^ ImageLibrary
    -> Game -- ^ The game state to render
    -> Gloss.Picture -- ^ A picture of this game state
  renderGame imgLib game = Gloss.pictures
    [ renderBackground (view backgroundImg imgLib)
    , renderSpaceship (view spaceshipImg imgLib) (spaceship game)
    , renderMonsters (view monsterImg imgLib) (monsters game)
    ]

  -- | Render the background image into a displayable 'Gloss.Picture'
  renderBackground
    :: Gloss.Picture  -- ^ Background Image
    -> Gloss.Picture -- ^ Background picture
  renderBackground bkgImg = bkgImg

  -- | Render the spaceship into a displayable 'Gloss.Picture'
  renderSpaceship
    :: Gloss.Picture -- ^ Spaceship Image
    -> Spaceship -- ^ Current spaceship (x,y) position
    -> Gloss.Picture -- ^ Picture of the spaceship
  renderSpaceship spaceshipImg (Spaceship (x, y)) =
    -- The picture of the spaceship is the corresponding library sprite translated
    -- by the spaceship coordinates.
    Gloss.translate x y $ spaceshipImg

  -- | Render a monster into a displayable 'Gloss.Picture'
  renderMonster
    :: Gloss.Picture -- ^ Monster image
    -> Monster -- ^ Monster (x,y) position
    -> Gloss.Picture -- ^ Picture of the monster
  renderMonster monsterImg (Monster (x, y)) =
    -- The picture of the monster is the corresponding library sprite translated
    -- by the spaceship coordinates.
    Gloss.translate x y $ monsterImg

  -- ***************** TODO (Suggestions only) ******************

  -- | Render multiple monsters in one go.
  renderMonsters
    :: Gloss.Picture -- ^ Monster image
    -> Monsters -- ^ Monsters positions.
    -> Gloss.Picture -- ^ Collage picture with all monsters represented.
  renderMonsters mstImg monsters = Gloss.pictures (fmap (renderMonster mstImg) monsters)
  -- Hint: think fmap, think currying !

  -- | Render score
  renderScores
    :: Int
    -> Gloss.Picture
  renderScores = undefined
  -- Hint : Use 'renderText' from Gloss.


  -- *********************** Key handling ************************

  -- | Modify 'Game' state based on key events.
  handleKeys
    :: Gloss.Event -- ^ keyEvent
    -> Game -- ^ current game state
    -> Game -- ^ Game updated
  handleKeys _ game = game
  -- Hint: pattern-match on event key parameter (see Gloss documentation).
