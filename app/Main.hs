module Main where

import SpaceInvaders

import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Interface.IO.Game (playIO)

import System.Environment (getArgs)

-- | Load image library. For simplicity's sake, this function assumes that the
-- game is run from the root project directory.
loadLibrary :: IO ImageLibrary
loadLibrary = do
  backgroundImg <- loadBMP "./assets/images/galaxy-2643089_960_720.bmp"
  spaceshipImg <- loadBMP "./assets/images/spaceship_resized2.bmp"
  monster1Img <- loadBMP "./assets/images/monster1_resized.bmp"
  return $ ImageLibrary
    { backgroundImg = backgroundImg
    , spaceshipImg = spaceshipImg
    , monster1Img = monster1Img
    }

main :: IO ()
main = do
  playerId:_ <- getArgs
  imageLibrary <- loadLibrary
  gameState <- mkInitialState imageLibrary playerId
  playIO
    window     -- Specification of game window       :: Window
    background -- Background color                   :: Color
    fps        -- Frames per second                  :: Int
    gameState  -- Initial game state                 :: Game
    renderGame -- Rendering function :: Game -> Picture
    handleKeysIO -- Key handling function              :: Event -> Game -> Game
    update     -- State update function              :: Float -> Game -> Game
