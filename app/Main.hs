module Main where

import SpaceInvaders
import GlossSpaceInvadersInterface

import Graphics.Gloss (loadBMP, play)

-- | Load image library. For simplicity's sake, this function assumes that the
-- game is run from the root project directory.
loadLibrary :: IO ImageLibrary
loadLibrary = do
  bkgImg <- loadBMP "./assets/images/galaxy-2643089_960_720.bmp"
  shipImg <- loadBMP "./assets/images/spaceship_resized2.bmp"
  mstImg <- loadBMP "./assets/images/monster1_resized.bmp"
  return $ ImageLibrary
    { _backgroundImg = bkgImg
    , _spaceshipImg = shipImg
    , _monsterImg = mstImg
    }

-- | main function
main :: IO ()
main = do
  imageLibrary <- loadLibrary
  play
    window     -- Specification of game window       :: Window
    background -- Background color                   :: Color
    fps        -- Frames per second                  :: Int
    gameInitialState  -- Initial game state                 :: Game
    (renderGame imageLibrary) -- Rendering function with the image Library  Game -> Picture
    handleKeys -- Key handling function              :: Event -> Game -> Game
    update     -- State update function              :: Float -> Game -> Game
