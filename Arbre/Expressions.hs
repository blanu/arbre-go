{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Arbre.Expressions
(
 Computation(..),
 ObjectDef(..),
 Object(..),
 State(..),
 Def(..),
 Block(..),
 Expression(..),
 Literal(..),
 Environment(..),
 Selector(..),
 Context(..),
 Mapping(..),
 MutationType(..),
 EventType(..),
 ReceiverType(..),
 Closure(..),
 Value(..),
 Effect(..),
 Symbol(..),
 Error(..),
 Agenda(..),
 Plan(..),
 fl,
 err
)
where

import Data.Typeable
import Data.Data
import Data.Map as M
import Control.Lens (makeLenses, makePrisms)

import Arbre.Box

data Interface = Interface [String]

data ObjectDef = ObjectDef {
    _objdef :: [Def]
  }
  deriving (Eq, Show, Typeable, Data)
  
data Def =
    Def {
      _defsym :: String,
      _defval :: Value
    }
  deriving (Eq, Show, Typeable, Data)

data Object =
      Module {_modobj :: ObjectDef}
    | Object {_objstate :: State, _objmethods :: ObjectDef}
    deriving (Eq, Show, Typeable, Data)
data State =
      ObjectState {_stateobj :: Object}
    | LiteralState {_stateLiteral :: Literal}
    deriving (Eq, Show, Typeable, Data)
    
data Block = Block {
    _args :: [String],
    _blockExp :: Expression
  }
  deriving (Eq, Show, Typeable, Data)

data Selector = Lex | Dyn | Self | Value | Local deriving (Eq, Show, Typeable, Data)
data Environment = Environment {
    _lex :: Mapping,
    _dyn :: Mapping,
    _self :: Mapping,
    _value :: Mapping
  }
  deriving (Eq, Show, Typeable, Data)

-- lex dyn self value local
data Context = Context {
    _environment :: Environment,
    _local :: Mapping
  }
  deriving (Eq, Show, Typeable, Data)

data Mapping = Mapping {
    _mappingMap :: (M.Map String Value)
  }
  deriving (Eq, Show, Typeable, Data)

data Computation = Computation {
    _context :: Context,
    _expression :: Expression
  }
  deriving (Eq, Show, Typeable, Data)

data Expression =
    Call {_callTarget :: Expression, _callSelector :: String, _callArgs :: [Expression]}
  | Apply {_applyBlock :: Block, _applyArgs :: [Expression]}
  | Eval {_evalClosure :: Closure, _evalArgs :: [Expression]}
  | Compute {_computeValue :: Computation}
  | NativeCall {_nativeCallType :: String, _nativeCallArgs :: [Expression]}
  | Symref {_sym :: Symbol}
  | Return {_retval :: Value}
  | Act {_action :: Action}
  | Failure {_error :: Error}
  deriving (Eq, Show, Typeable, Data)

data Symbol = Symbol {
    _selector :: Selector,
    _symval :: String
  }
  deriving (Eq, Show, Typeable, Data)

data Error = Error {_errorString :: String} deriving (Eq, Show, Typeable, Data)

data Value =
    ObjectValue {_valObj :: Object}
  | EffectValue {_valEff :: Effect}
  | BlockValue {_valBlock :: Block}
  | ClosureValue {_valClosure :: Closure}
  | Symdef {_valSym :: Symbol}
  | ErrorValue {_valError :: Error}
  | AgendaValue {_valAgenda :: Agenda}
  deriving (Eq, Show, Typeable, Data)

data Closure = Closure {
  _closureEnv :: Environment,
  _block :: Block
 }
 deriving (Eq, Show, Typeable, Data) -- lex dyn self value

data Action =
    Mutate {_mutEff :: Effect, _mutClosure :: Closure}
  | Emit {_emitEff :: Effect, _emitClosure :: Maybe Closure}
  | Receive {_recEff :: Effect, _recClosure :: Closure}
  deriving (Eq, Show, Typeable, Data)

data Effect =
    Mutation {_mutType :: MutationType, _mutSym :: Symbol, _mutVal :: Value, _effMutClosure :: Closure}
  | Event {_eventType :: EventType, _eventVal :: Value, _eventClosure :: Maybe Closure}
  | Receiver {_recType :: ReceiverType, _effRecClosure :: Closure} deriving (Eq, Show, Typeable, Data)

data MutationType =
    Define
  | Set
  deriving (Eq, Show, Typeable, Data)

data EventType =
    Print
  | Terminate
  deriving (Eq, Show, Typeable, Data)

data ReceiverType =
  Stdin
  deriving (Eq, Show, Typeable, Data)

data Agenda = Agenda {
    _agendaPlan :: Plan,
    _agendaValue :: Expression
  }
  deriving (Eq, Show, Typeable, Data)

data Plan =
    Send
  | SendThen {
      _sendThenBlock :: Agenda
    }
  | Wait {
      _waitReceiver :: Block,
      _waitPlan :: Plan
    }
  | Iterate
  | Fail
  | Display
  | DisplayThen {
      _printThenBlock :: Agenda
    }
  | Condition {
      _conditionEmit :: Plan,
      _conditionMutate :: Plan,
      _conditionReceive :: Plan,
      _conditionError :: Plan,
      _conditionValue :: Plan
    }
  | Split {
      _splitLeft :: Plan,
      _splitRight :: Plan
  }
    deriving (Eq, Show, Typeable, Data)

fl :: String -> Expression
fl s = Failure $ Error s

err :: String -> Value
err s = ErrorValue $ Error s

makeLenses ''ObjectDef

makePrisms ''Def
makeLenses ''Def

makeLenses ''Object
makePrisms ''Object

makeLenses ''State
makePrisms ''State

makeLenses ''Block

makePrisms ''Selector

makeLenses ''Environment
makeLenses ''Context
makeLenses ''Mapping
makeLenses ''Computation

makeLenses ''Expression
makePrisms ''Expression

makeLenses ''Symbol
makeLenses ''Error

makeLenses ''Value
makePrisms ''Value

makeLenses ''Closure

makeLenses ''Action
makePrisms ''Action

makeLenses ''Effect
makePrisms ''Effect

makePrisms ''MutationType
makePrisms ''EventType
makePrisms ''ReceiverType

makePrisms ''Plan
makeLenses ''Plan
