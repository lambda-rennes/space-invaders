module ImageLibrary
  ( Library(..)
  , loadLibrary
  ) where

import Graphics.Gloss

-- | Image library
data Library = Library
  { backgroundImg :: Picture
  , spaceshipImg :: Picture
  , monster1Img :: Picture
  }

-- | Load image library. For simplicity's sake, this function assumes that the
-- game is run from the root project directory.
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
