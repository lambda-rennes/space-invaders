{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Message
  ( Message(..)
  ) where

import Data.BERT

data Message
  = NewPosition Float

instance BERT Message where

  showBERT :: Message -> Term
  showBERT msg = case msg of
    NewPosition pos -> showBERT ("position", pos)

  readBERT :: Term -> Either String Message
  readBERT t = do
    (tag, payload) <- readBERT @(String, Term) t
    case tag of
      "newpos" -> NewPosition <$> readBERT @Float payload
      _ -> Left $ "Unexpected tag: " ++ tag
