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
    , renderedLasers
    , renderedShip
    , renderedInvaders
    ]
    where
      renderedBkg = renderBackground (backgroundImg imgLib)
      renderedShip = renderSpaceship (spaceship game) (spaceshipImg imgLib)
      renderedInvaders = renderInvaders (invaders game) (invaderImg imgLib)
      renderedLasers = renderLasers (lasers game)

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
    :: Invaders -- ^ Invaders positions.
    -> Gloss.Picture -- ^ Invader image
    -> Gloss.Picture -- ^ Collage picture with all Invaders represented.
  renderInvaders invs img = Gloss.pictures $ fmap renderInvaderWithImg invs
    where renderInvaderWithImg = renderInvader img
  -- apply the pictures method to each Invader in Invaders (list of Invader)

  --Render a laser list by using map and renderLaser
  --Gloss.pictures take a list of pictures and join them.
  renderLasers :: [Laser] -> Gloss.Picture
  renderLasers lasers = Gloss.pictures $ map renderLaser lasers

  --Render a laser by using a primitive
  renderLaser :: Laser -> Gloss.Picture
  renderLaser (Laser (x, y)) = Gloss.translate x y $ Gloss.color Gloss.red $ Gloss.circleSolid 15

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
  fromGlossEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) Gloss.Down _ _) = Just LeftKey
  fromGlossEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) Gloss.Down _ _) = Just RightKey
  fromGlossEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) Gloss.Down _ _) = Just PiouPiou
  fromGlossEvent _ = Nothing

  -- | call domain handleActionKeys function if a key is known
  handleKey :: Maybe GameKey -> Game -> Game
  handleKey Nothing game = game
  handleKey (Just k) game = handleActionKeys k game
