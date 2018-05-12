module Main where

import Lib
import GameBoard

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game


play :: Display -- ^ Window
     -> Color -- ^ Background color
     -> Int -- ^ frame per second
     -> Game -- ^ gme initial state
     -> (Game -> Picture) -- ^ rendering function take a Game and return a picture of the game
     -> (Event -> Game -> Game) -- ^ function take an event and a game and return the game updated
     -> (Float -> Game -> Game) -- ^ function tame seconds since last update and a game and return the game state updated

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
