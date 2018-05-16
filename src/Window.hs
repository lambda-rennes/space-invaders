module Window
  ( fps
  , background
  , window
  ) where

import Graphics.Gloss

-- | Position of the window on the screen (WM hint only).
winPos :: (Int, Int)
winPos = (100, 100)

-- | Window dimensions.
winSize :: (Int, Int)
winSize = (960, 640)

-- | Backround color.
background :: Color
background = greyN 0.1

-- | Window specification (title, size and position).
window :: Display
window = InWindow "Haskell Space Invaders" winSize winPos

-- | Target frames per second (Gloss hint).
fps :: Int
fps = 60
