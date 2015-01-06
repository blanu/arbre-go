{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Receiver
(
  applyReceiver
)
where

import Arbre.Expressions
import Arbre.View
import Arbre.Native
import Arbre.Context as C
import Arbre.Box

applyReceiver :: ReceiverType -> Closure -> Context -> IO(Computation)
applyReceiver Stdin closure context  = do
  c <- getChar
  let s = [c] :: String
  let s' = boxString s
  return $ Computation context (Eval closure [Return s'])
