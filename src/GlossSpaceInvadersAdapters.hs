module GlossSpaceInvadersAdapters
  ( ImageLibrary(..)
  , renderGame
  , fromGlossEvent
  , renderScores
  , mbHandleAction
  , update
    -- Re-exports.
  , module Window
  ) where

  import SpaceInvaders


  import qualified Graphics.Gloss as Gloss
  import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
  import Window

  -- gloss Picture documentation : https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss-Data-Picture.html


  -- *************** Gloss SpaceInvaders Interface ****************


  -- | Image library record
  data ImageLibrary = ImageLibrary
    { backgroundImg :: Gloss.Picture
    , spaceshipImg :: Gloss.Picture
    , invaderImg :: Gloss.Picture
    }

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
    , renderedBullets
    ]
    where
      renderedBullets = Gloss.pictures $ fmap renderBullet (bullets game)
      renderedBkg = renderBackground (backgroundImg imgLib)
      renderedShip = renderSpaceship (spaceship game) (spaceshipImg imgLib)
      renderedInvaders = renderInvaders (invaders game) (invaderImg imgLib)

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
    Gloss.translate x y shipImg

  -- | Render a Invader into a displayable 'Gloss.Picture'
  renderInvader
    :: Gloss.Picture -- ^ Invader image
    -> Invader -- ^ Invader (x,y) position
    -> Gloss.Picture -- ^ Picture of the Invader
  renderInvader img (Invader (x, y)) =
    -- The picture of the Invader is the corresponding library sprite translated
    -- by the spaceship coordinates.
    Gloss.translate x y img

  -- | Render multiple Invaders in one go.
  renderInvaders
    :: Invaders
    -> Gloss.Picture -- ^ Invader image
    -> Gloss.Picture -- ^ Collage picture with iall Invaders represented.
  renderInvaders invs img = Gloss.pictures $ fmap renderInvaderWithImg invs
    where renderInvaderWithImg = renderInvader img
  -- apply the pictures method to each Invader in Invaders (list of Invader)


  renderBullet :: Bullet -> Gloss.Picture
  renderBullet (Bullet (x, y)) =  Gloss.translate x y $ Gloss.Color Gloss.red $ Gloss.circleSolid 10   

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

  -- | Convert gloss event keys to game actions
  fromGlossEvent :: Gloss.Event -> Maybe GameAction
  fromGlossEvent (Gloss.EventKey (Gloss.Char 'r') Gloss.Down _ _) = Just ResetAction
  fromGlossEvent (Gloss.EventKey (Gloss.Char 'q') Gloss.Down _ _) = Just LeftAction
  fromGlossEvent (Gloss.EventKey (Gloss.Char 'd') Gloss.Down _ _) = Just RightAction
  fromGlossEvent (Gloss.EventKey (Gloss.Char 'x') Gloss.Down _ _) = Just ShootAction
  fromGlossEvent _ = Nothing

  -- | call domain handleAction function if a key is known
  mbHandleAction :: Maybe GameAction -> Game -> Game
  mbHandleAction Nothing game = game
  mbHandleAction (Just k) game = handleAction k game
