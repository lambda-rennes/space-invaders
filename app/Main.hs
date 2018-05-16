module Main where

import SpaceInvaders

import Graphics.Gloss (play)

main :: IO ()
main = do
  imageLibrary <- loadLibrary
  let gameState = mkInitialState imageLibrary
  play
    window     -- Specification of game window       :: Window
    background -- Background color                   :: Color
    fps        -- Frames per second                  :: Int
    gameState  -- Initial game state                 :: Game
    renderGame -- Rendering function                 :: Game -> Picture
    handleKeys -- Key handling function              :: Event -> Game -> Game
    update     -- State update function              :: Float -> Game -> Game
