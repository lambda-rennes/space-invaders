{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Message
  ( Message(..)
  ) where

import Data.BERT

data Message
  = NewPosition Float
  | Missile Float

instance BERT Message where

  showBERT :: Message -> Term
  showBERT msg = case msg of
    NewPosition pos -> showBERT ("newpos", show pos)
    Missile pos -> showBERT ("missile", show pos)

  readBERT :: Term -> Either String Message
  readBERT t = do
    (tag, payload) <- readBERT @(String, Term) t
    case tag of
      "newpos" -> NewPosition . read <$> readBERT payload
      "missile" -> Missile . read <$> readBERT payload
      _ -> Left $ "Unexpected tag: " ++ tag
