{-# LANGUAGE TemplateHaskell #-}

module GlossSpaceInvadersInterface
  ( ImageLibrary(..)
  , SpaceshipImg(..)
  , MonsterImg(..)
  , BackgroundImg(..)
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

  type SpaceshipImg = Gloss.Picture
  type MonsterImg = Gloss.Picture
  type BackgroundImg = Gloss.Picture


  -- | Image library record
  data ImageLibrary = ImageLibrary
    { _backgroundImg :: BackgroundImg
    , _spaceshipImg :: SpaceshipImg
    , _monsterImg :: MonsterImg
    }

  makeLenses ''ImageLibrary -- ^ needed to access easily to the record attr


  -- *********************** Rendering *****************************

  renderGame
  -- | Render the 'Game' into a displayable 'Gloss.Picture'.
    :: ImageLibrary -- ^ ImageLibrary
    -> Game -- ^ The game state to render
    -> Gloss.Picture -- ^ A picture of this game state
  renderGame imgLib game = Gloss.pictures
    [ renderBackground imgLib
    , renderSpaceship (view spaceshipImg imgLib) (spaceship game)
    , renderMonsters (view monsterImg imgLib) (monsters game)
    ]

  -- | Render the background image into a displayable 'Gloss.Picture'
  renderBackground
    :: ImageLibrary -- ^ Image library
    -> Gloss.Picture -- ^ Background picture
  renderBackground library = view backgroundImg library

  -- | Render the spaceship into a displayable 'Gloss.Picture'
  renderSpaceship
    :: SpaceshipImg -- ^ Spaceship Image
    -> Spaceship -- ^ Current spaceship (x,y) position
    -> Gloss.Picture -- ^ Picture of the spaceship
  renderSpaceship spaceshipImg (Spaceship (x, y)) =
    -- The picture of the spaceship is the corresponding library sprite translated
    -- by the spaceship coordinates.
    Gloss.translate x y $ spaceshipImg

  -- | Render a monster into a displayable 'Gloss.Picture'
  renderMonster
    :: MonsterImg -- ^ Monster image
    -> Monster -- ^ Monster (x,y) position
    -> Gloss.Picture -- ^ Picture of the monster
  renderMonster monsterImg (Monster (x, y)) =
    -- The picture of the monster is the corresponding library sprite translated
    -- by the spaceship coordinates.
    Gloss.translate x y $ monsterImg

  -- ***************** TODO (Suggestions only) ******************

  -- | Render multiple monsters in one go.
  renderMonsters
    :: MonsterImg -- ^ Monster image
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
