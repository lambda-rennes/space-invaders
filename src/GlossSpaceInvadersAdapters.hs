{-# LANGUAGE TemplateHaskell #-}

module GlossSpaceInvadersAdapters
  ( ImageLibrary(..)
  , renderGame
  , fromGlossEvent
  , renderScores
  , handleKey
  , update
    -- Re-exports.
  , module Window
  ) where

  import SpaceInvaders

  import Control.Lens

  import qualified Graphics.Gloss as Gloss
  --import qualified Graphics.Gloss.Data.Color as Gloss
  import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
  import Window

  -- gloss Picture documentation : https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss-Data-Picture.html


  -- *************** Gloss SpaceInvaders Interface ****************

  -- | Image library record
  data ImageLibrary = ImageLibrary
    { _backgroundImg :: Gloss.Picture
    , _spaceshipImg :: Gloss.Picture
    , _invaderImg :: Gloss.Picture
    }

  makeLenses ''ImageLibrary -- needed to access easily to the record attr


  -- *********************** Rendering *****************************

  -- | Render the 'Game' into a displayable 'Gloss.Picture'.
  renderGame
    :: ImageLibrary -- ^ ImageLibrary
    -> Game -- ^ The game state to render
    -> Gloss.Picture -- ^ A picture of this game state
  renderGame imgLib game = Gloss.pictures
    [ renderedBkg
    , renderedShip
    , renderedInvaders
    , renderedMissiles
    ]
    where
      renderedBkg = renderBackground (view backgroundImg imgLib)
      renderedShip = renderSpaceship (view spaceship game) (view spaceshipImg imgLib)
      renderedInvaders = renderInvaders (view invaders game) (view invaderImg imgLib)
      renderedMissiles = Gloss.pictures (fmap renderMissile (view missiles game))
      renderMissile (Missile (x,y)) =
        Gloss.translate x y (Gloss.color Gloss.yellow (Gloss.circleSolid 10) ) 

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
  renderSpaceship (Spaceship (x, y)) shipImg = (Gloss.translate x y)  shipImg
    
    -- The picture of the spaceship is the corresponding library sprite translated
    -- by the spaceship coordinates.
    

  -- | Render a Invader into a displayable 'Gloss.Picture'
  renderInvader
    :: Gloss.Picture -- ^ Invader image
    -> Invader -- ^ Invader (x,y) position
    -> Gloss.Picture -- ^ Picture of the Invader
  renderInvader img (Invader (x, y)) = Gloss.translate x y img
    
    -- The picture of the Invader is the corresponding library sprite translated
    -- by the spaceship coordinates.


  -- | Render multiple Invaders in one go.
  renderInvaders
    :: Invaders -- ^ Invaders positions.
    -> Gloss.Picture -- ^ Invader image
    -> Gloss.Picture -- ^ Collage picture with all Invaders represented.
  renderInvaders invs img = Gloss.pictures  (fmap renderInvaderWithImg invs)
    where renderInvaderWithImg = renderInvader img
  
  -- apply the pictures method to each Invader in Invaders (list of Invader)


  -- ***************** TODO (Suggestions only) ******************

  -- | TODO Render score
  renderScores
    :: Int
    -> Gloss.Picture
  renderScores = undefined
  -- Hint : Use 'renderText' from Gloss.

  -- |  TODO Render missile lauch by spaceship
  --renderMissile
  --  :: need a new type?
  --  -> Gloss.Picture // what the missile look like
  --renderMissile = undefined
  -- Hint : Use 'polygon' or 'circle' from Gloss.


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
  fromGlossEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) Gloss.Down _ _ ) = Just MoveRightKey
  fromGlossEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) Gloss.Down _ _ ) = Just MoveLeftKey
  fromGlossEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) Gloss.Down _ _ ) = Just ShootKey

  fromGlossEvent _ = Nothing

  -- | call domain handleActionKeys function if a key is known
  handleKey :: Maybe GameKey -> Game -> Game
  handleKey Nothing game = game
  handleKey (Just k) game = handleActionKeys k game
