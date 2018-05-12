{-# LANGUAGE ScopedTypeVariables #-}

module GameBoard
  ( Game
  , handleKeys
  , loadLibrary
  , renderGame
  , initialState
  , update
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib


-- *********************** LIBRARY *******************************

-- | Images library
data Library = Library
  { backgroundImg :: Picture
  , spaceshipImg :: Picture
  , monster1Img :: Picture
  }

-- *********************** GAME STATE ****************************

-- | Game type
newtype Game = Game
  { library :: Library
  }

-- | spaceship
newtype Spaceship = Spaceship
  { pos :: (Float, Float)
  }

-- | initial game state
initialState :: Library -- ^ image library
            -> Game -- ^ initial game state
initialState l =
  Game
  {
    library = l -- ^ put the library passed and put it in the game state
  }

-- *********************** Updating game ************************

-- | Update the game state
update ::
     Float -- ^ The number of seconds since last update
  -> Game -- ^ current game state
  -> Game -- ^ A new game state with an updated ball and paddles positions.
-- Game playing
update seconds game = moveSpaceship $ game

-- | move spaceship
moveSpaceship ::
       Game -- ^ Game state to update
    -> Game -- ^ Game updated
moveSpaceship game = game
-- hint : record update

-- *********************** HANDELING KEYS ************************

-- | Pure responding to key events.
handleKeys ::
     Event -- ^ keyEvent
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
handleKeys _ game = game
-- hint pattern match on event key (Gloss)

-- *********************** RENDERING *****************************

-- | render the game
renderGame ::
     Game -- ^ The game state to render
  -> Picture -- ^ A picture of this game state
renderGame game =
  pictures
  [ renderBackground (library game)
  , renderSpaceship (library game) (0, -250)
  , renderMonster (library game) (0, 250)
  ]

-- | render background galaxy image
renderBackground ::
      Library -- ^ image library
   -> Picture -- ^ picture of the background
renderBackground library = backgroundImg library

-- | render the spaceship
renderSpaceship ::
      Library -- ^image library
   -> (Float, Float) -- ^ spaceship position
   -> Picture -- ^ picture of the spaceship
-- 2 arguments library and the tuple = Translating and putting the picture of the spaceship
renderSpaceship library (x, y) = translate x y $ spaceshipImg library

-- | render a monster
renderMonster ::
      Library -- ^ image library
      -> (Float, Float) -- ^ monster position
      -> Picture -- ^ picture of the monster
-- 2 arguments library and the tuple = Translating and putting the picture of the monster
renderMonster library (x, y) = translate x y $ monster1Img library

-- *********************************************************************
-- ******** To do ******************************************************
-- *********************************************************************

-- | render multiple monster1
renderMonsters ::
      [(Float, Float)] -- ^ monsters positions
      -> Picture       -- ^ picture with all monsters
renderMonsters = undefined
-- | hint : curry


-- | render score
renderScores ::
      Int
      -> Picture
renderScores = undefined
-- | Hint : renderText from Gloss
