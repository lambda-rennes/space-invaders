{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Message
  ( Message(..)
  ) where

import Data.BERT

data Message
  = NewPosition String -- XXX: Just because.

instance BERT Message where

  showBERT :: Message -> Term
  showBERT msg = case msg of
    NewPosition pos -> showBERT ("newpos", pos)

  readBERT :: Term -> Either String Message
  readBERT t = do
    (tag, payload) <- readBERT @(String, Term) t
    case tag of
      "newpos" -> NewPosition <$> readBERT payload
      _ -> Left $ "Unexpected tag: " ++ tag
