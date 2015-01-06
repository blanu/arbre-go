module Arbre.Mutation
(
  applyMutation
)
where

import Arbre.Context
import Arbre.Expressions

applyMutation :: Effect -> Closure
applyMutation (Mutation Define (Symbol Dyn sym) value closure@(Closure env block)) = do
  let context = open closure
  let context' = bindPair Dyn context (sym, value)
  close context' block
applyMutation (Mutation Set (Symbol Dyn sym) value closure@(Closure env block)) = do
  let context = open closure
  let context' = bindPair Dyn context (sym, value)
  close context' block
