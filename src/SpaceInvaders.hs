module SpaceInvaders
    ( Game (..)
    , Invaders
    , Invader (..)
    , Spaceship (..)
    , GameKey (..)
    , handleActionKeys
    , gameInitialState
    , update
    , moveInvader
    ) where

-- *********************** Game domain ****************************

-- | Elapsed time alias (seconds since last cycle)
type ElapsedTime = Float
-- | Position type alias
type Position = (Float, Float)
-- | Spaceship type
newtype Spaceship = Spaceship Position
-- | Invader type
newtype Invader = Invader Position
-- | Invader type alias
type  Invaders = [Invader]
-- | Game possible keys
data GameKey = ResetKey
-- | Game record
data Game = Game
  { spaceship :: Spaceship
  , invaders :: Invaders
  }

-- | Create the initial game state of the game
gameInitialState
  :: Game    -- ^ Initial game state
gameInitialState = Game
  { spaceship = Spaceship (0, -250)
  , invaders = [Invader (0, 250)]
  }

-- *********************** Updating game ************************

-- | Update the 'Game' since last frame.
update
  :: ElapsedTime -- ^ Time passed since last update
  -> Game -- ^ Current game state
  -> Game -- ^ Updated game state.
update _ game@Game {spaceship = sp, invaders = invs} = game {spaceship = updateSpaceship, invaders = updateInvaders}
  where
    updateSpaceship = moveSpaceship sp
    updateInvaders = moveInvaders invs
    -- TODO need to move invaders too...


-- | Modify 'Game' state based on GameKeys.
handleActionKeys
  :: GameKey -- ^ gameKey to handle
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleActionKeys ResetKey _ = gameInitialState
-- TODO if you need an other game key
-- Hint: pattern-match on the GameKey type


-- ***************** TODO (Suggestions only) ******************

-- | TODO Move spaceship.
moveSpaceship
  :: Spaceship -- ^ initial Spaceship
  -> Spaceship -- ^ Spaceship updated
moveSpaceship = id -- identity - we do nothing
-- Hint : implement the function to move spaceship (maybe you need to change the signature)

-- | TODO Move invader.
moveInvader
  :: Invader -- ^ initial invader
  -> Invader -- ^ invader updated
moveInvader = id -- identity - we do nothing

-- | TODO Move invaders.
moveInvaders
  :: Invaders -- ^ initial list of invaders
  -> Invaders -- ^ list of invaders updated
moveInvaders = id -- identity - we do nothing
-- Hint :  implement function to move a list of invaders with fmap
