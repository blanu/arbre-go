{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Context
(
  empty,
  emptyContext,
  resolve,
  addDynamicPair,
  chainMapping,
  unwrapDef,
  bindPair,
  stack,
  close,
  open,
  error
)
where

import Data.Typeable
import Data.Data
import qualified Data.Map as M

import Arbre.Expressions

unwrapDef :: Def -> (String, Value)
unwrapDef (Def name exp) = (name,exp)

chainMapping :: Mapping -> Mapping -> Mapping
--chainMapping (Mapping lex) (Mapping newLex) = Mapping $ M.union lex newLex
chainMapping (Mapping lex) (Mapping newLex) = Mapping $ M.union newLex lex

empty :: Mapping
empty = Mapping M.empty

emptyContext :: Context
emptyContext = Context (Environment empty empty empty empty) empty

resolve :: Selector -> String -> Context -> Value
resolve Lex   key (Context (Environment lex dyn self value) local) = maplookup key lex
resolve Dyn   key (Context (Environment lex dyn self value) local) = maplookup key dyn
resolve Self  key (Context (Environment lex dyn self value) local) = maplookup key self
resolve Value key (Context (Environment lex dyn self value) local) = maplookup key value
resolve Local key (Context (Environment lex dyn self value) local) = maplookup key local

maplookup :: String -> Mapping -> Value
maplookup key (Mapping map) =
  let ref = M.lookup key map
    in case ref of
      Just value -> value
      Nothing -> err $ "Undefined reference " ++ key ++": " ++ show (M.keys map)

bind :: Selector -> String -> Value -> Context -> Context
bind Lex   key val (Context (Environment lex dyn self value) local) = Context (Environment (bindMapping key val lex) dyn self value) local
bind Dyn   key val (Context (Environment lex dyn self value) local) = Context (Environment lex (bindMapping key val dyn) self value) local
bind Self  key val (Context (Environment lex dyn self value) local) = Context (Environment lex dyn (bindMapping key val self) value) local
bind Value key val (Context (Environment lex dyn self value) local) = Context (Environment lex dyn self (bindMapping key val value)) local
bind Local key val (Context (Environment lex dyn self value) local) = Context (Environment lex dyn self value) (bindMapping key val local)

bindMapping :: String -> Value -> Mapping -> Mapping
bindMapping key value (Mapping map) = Mapping $ M.insert key value map

bindPair :: Selector -> Context -> (String, Value) -> Context
bindPair Lex   (Context (Environment lex dyn self value) local) (key, val) = Context (Environment (bindPairMapping (key, val) lex) dyn self value) local
bindPair Dyn   (Context (Environment lex dyn self value) local) (key, val) = Context (Environment lex (bindPairMapping (key, val) dyn) self value) local
bindPair Self  (Context (Environment lex dyn self value) local) (key, val) = Context (Environment lex dyn (bindPairMapping (key, val) self) value) local
bindPair Value (Context (Environment lex dyn self value) local) (key, val) = Context (Environment lex dyn self (bindPairMapping (key, val) value)) local
bindPair Local (Context (Environment lex dyn self value) local) (key, val) = Context (Environment lex dyn self value) (bindPairMapping (key, val) local)

bindPairMapping :: (String, Value) -> Mapping -> Mapping
bindPairMapping (key, value) (Mapping map) = Mapping $ M.insert key value map

addDynamicPair :: Mapping -> (String, Value) -> Mapping
addDynamicPair (Mapping dyn) (key, value) = Mapping $ M.insert key value dyn

stack :: Context -> Context
stack (Context (Environment lex dyn self value) local) = Context (Environment (chainMapping lex local) dyn self value) empty

close :: Context -> Block -> Closure
close (Context (Environment lex dyn self value) local) block@(Block _ _) = Closure (Environment (chainMapping lex local) dyn self value) block

open :: Closure -> Context
open (Closure env _) = Context env empty
