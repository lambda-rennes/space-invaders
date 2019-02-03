{-# LANGUAGE TemplateHaskell #-}

module GlossSpaceInvadersAdapters
  ( ImageLibrary(..)
  , renderGame
  , fromGlossEvent
  , renderScores
  , update
    -- Re-exports.
  , module Window
  ) where

  import SpaceInvaders

  import Control.Lens

  import qualified Graphics.Gloss as Gloss
  import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
  import Window

  -- gloss Picture documentation : https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss-Data-Picture.html


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
    , renderSpaceship (spaceship game) (view spaceshipImg imgLib)
    , renderMonsters (monsters game) (view monsterImg imgLib)
    ]

  -- | Render the background image into a displayable 'Gloss.Picture'
  renderBackground
    :: Gloss.Picture  -- ^ Background Image
    -> Gloss.Picture -- ^ Background picture
  renderBackground bkgImg = bkgImg

  -- | Render the spaceship into a displayable 'Gloss.Picture'
  renderSpaceship
    :: Spaceship -- ^ Current spaceship (x,y) position
    -> Gloss.Picture -- ^ Spaceship Image
    -> Gloss.Picture -- ^ Picture of the spaceship placed in the world
  renderSpaceship (Spaceship (x, y)) shipImg =
    -- The picture of the spaceship is the corresponding library sprite translated
    -- by the spaceship coordinates.
    Gloss.translate x y $ shipImg

  -- | Render a monster into a displayable 'Gloss.Picture'
  renderMonster
    :: Gloss.Picture -- ^ Monster image
    -> Monster -- ^ Monster (x,y) position
    -> Gloss.Picture -- ^ Picture of the monster
  renderMonster mstImg (Monster (x, y)) =
    -- The picture of the monster is the corresponding library sprite translated
    -- by the spaceship coordinates.
    Gloss.translate x y $ mstImg

  -- ***************** TODO (Suggestions only) ******************

  -- | Render multiple monsters in one go.
  renderMonsters
    :: Monsters -- ^ Monsters positions.
    -> Gloss.Picture -- ^ Monster image
    -> Gloss.Picture -- ^ Collage picture with all monsters represented.
  renderMonsters msts mstImg = Gloss.pictures $ fmap (renderMonster mstImg) msts
  -- Hint: think fmap, think currying !

  -- | Render score
  renderScores
    :: Int
    -> Gloss.Picture
  renderScores = undefined
  -- Hint : Use 'renderText' from Gloss.


  -- *********************** Key handling ************************

  -- EventKey Key KeyState Modifiers (Float, Float)
  -- EventKey documentation : https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss-Interface-IO-Game.html#t:Event
  -- data KeyState = Up | Down
  -- data Key = Char Char | SpecialKey SpecialKey | MouseButton MouseButton
  -- Key documentation : https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss-Interface-IO-Game.html#t:Key
  -- SpecialKey documentation : https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss-Interface-IO-Game.html#t:SpecialKey

  -- ***************************************************************

  -- | Convert game keys to gloss event keys
  fromGlossEvent :: Gloss.Event -> Maybe GameKey
  fromGlossEvent (Gloss.EventKey (Gloss.Char 'r') Gloss.Down _ _) = Just ResetKey
  fromGlossEvent _ = Nothing
