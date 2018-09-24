module Message
  ( Message(..)
  ) where

import Data.BERT

data Message
  = Position Float

instance BERT Message where
  showBERT msg = case msg of
    Position pos -> showBERT ("position", pos)

  readBERT t = do
    (tag, payload) <- readBERT t
    case tag of
      "position" -> Position <$> readBERT payload
      _ -> Left $ "Unexpected tag: " ++ tag

