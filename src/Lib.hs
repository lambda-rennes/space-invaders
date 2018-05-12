module Lib
    ( loadLibrary
    , window
    , fps
    ) where


import GameBoard

-- | load library
loadLibrary :: IO Library
loadLibrary = do
  backgroundImg <- loadBMP "./library/galaxy-2643089_960_720.bmp"
  spaceshipImg <- loadBMP "./library/spaceship_resized2.bmp"
  monster1Img <- loadBMP "./library/monster1_resized.bmp"
  return
    Library
      { backgroundImg = backgroundImg
      , spaceshipImg = spaceshipImg
      , monster1Img = monster1Img
      }


-- | position of the window on the screen
offset :: Int
offset = 100

-- | Window dimension
winWidth, winHeight :: Int
winWidth = 960
winHeight = 640


-- | Window background Color
background :: Color
background = greyN 0.1

-- | Window
window :: Display
window = InWindow "Haskell Space Invaders" (winWidth, winHeight) (offset, offset)

-- | Frames per second
fps :: Int
fps = 60
