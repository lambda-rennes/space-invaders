module ImageLibrary
  ( ImageLibrary(..)
  , loadLibrary
  ) where

import Graphics.Gloss

-- | Image library
data ImageLibrary = ImageLibrary
  { backgroundImg :: Picture
  , spaceshipImg :: Picture
  , monster1Img :: Picture
  }

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
