module Main where

import GameLib
import GameBoard

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = do
  library <- loadLibrary
  play
    window -- ^ game window
    background -- ^ game background color
    fps -- ^ frame per second
    (initialState library) -- ^ initial game state
                           -- ^ passing an argument make the same function with an argument fixed
                           -- ^ initialState :: Library -> Game is transformed in initialState :: Game
    renderGame -- ^ render functionn
    handleKeys -- ^ handleKeys function
    update  -- ^ update function
