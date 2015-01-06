{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}

module Arbre.Native
(
  numdef,
  booldef,
  stringdef,
  Boxable,
  boxString,
  unboxString,
  applyNative
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Bimap (Bimap, (!), (!>))
import qualified Data.Bimap as Bi
import Data.Typeable
import Data.Data
import Debug.Trace

import Arbre.Expressions
import Arbre.Box
import Arbre.Context
import Arbre.View

type Native = ([Value] -> Expression)

fromList :: [(String,Value)] -> Mapping
fromList list = Mapping $ M.fromList list

applyNative :: String -> Native
applyNative "+" = add
applyNative "-" = arbre_subtract
applyNative "*" = mult
applyNative "/" = divide
applyNative "==" = eq
applyNative ">" = gt
applyNative "<" = lt
applyNative "+f" = float_add
applyNative "-f" = float_subtract
applyNative "*f" = float_mult
applyNative "/f" = float_divide
applyNative "==f" = float_eq
applyNative ">f" = float_gt
--applyNative FloatLessThan = float_lt
applyNative "==s" = string_eq
applyNative "append" = string_append
--applyNative BooleanEquals = boolean_eq
applyNative "if" = arbre_if
applyNative "and" = arbre_and
applyNative "or" = arbre_or
applyNative "not" = arbre_not

builtinParams :: (M.Map String [String])
builtinParams = M.fromList [
    ("+", ["a", "b"]),
    ("-", ["a", "b"]),
    ("*", ["a", "b"]),
    ("/", ["a", "b"]),
    ("==", ["a", "b"]),
    ("if", ["cond", "then", "else"]),
    (">", ["a", "b"]),
    ("<", ["a", "b"]),
    ("and", ["a", "b"]),
    ("or", ["a", "b"]),
    ("not", ["a"]),
    ("+f", ["a", "b"]),
    ("-f", ["a", "b"]),
    ("*f", ["a", "b"]),
    ("/f", ["a", "b"]),
    ("==f", ["a", "b"]),
    (">f", ["a", "b"]),
    ("append", ["a", "b"]),
    ("==s", ["a", "b"]),
    ("emit", ["type", "value"]),
    ("emitThen", ["type", "value", "next"]),
    ("mutate", ["type", "selector", "value"]),
    ("receive", ["type", "block"])
  ]

functionsToObject :: [String] -> ObjectDef
functionsToObject methods = ObjectDef []

numdef :: ObjectDef
numdef = functionsToObject ["+", "*", "==", ">"]

floatdef :: ObjectDef
floatdef = functionsToObject ["+f", "-f", "*f", "/f", "==f", ">f"]

booldef :: ObjectDef
booldef = functionsToObject ["and", "or"]

stringdef :: ObjectDef
stringdef = functionsToObject ["append", "==s"]

add :: Native
add params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> Return $ box (i+j)
add params = integerError params

arbre_subtract :: Native
arbre_subtract params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> Return $ box (i-j)
arbre_subtract params = integerError params

mult :: Native
mult params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> Return $ box (i*j)
mult params = integerError params

divide :: Native
divide params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> Return $ box (i `div` j)
divide params = integerError params

eq :: Native
eq params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> Return $ box (i==j)
eq params = integerError params

gt :: Native
gt params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> Return $ box (i>j)
gt params = integerError params

lt :: Native
lt params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> Return $ box (i<j)
lt params = integerError params

float_add :: Native
float_add params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> Return $ box (i+j)
float_add params = floatError params

float_subtract :: Native
float_subtract params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> Return $ box (i-j)
float_subtract params = floatError params

float_mult :: Native
float_mult params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> Return $ box (i*j)
float_mult params = floatError params

float_divide :: Native
float_divide params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> Return $ box (i / j)
float_divide params = floatError params

float_eq :: Native
float_eq params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> Return $ box (i==j)
float_eq params = floatError params

float_gt :: Native
float_gt params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> Return $ box (i>j)
float_gt params = floatError params

arbre_if :: Native
arbre_if params@(c:(ClosureValue t):(ClosureValue e):[]) = do
    let cond = unbox c :: Maybe Bool
    case cond of
        Nothing -> booleanError params
        Just True  -> Eval t []
        Just False -> Eval e []
arbre_if params = typeError params -- FIXME

arbre_and :: Native
arbre_and params@(a:b:[]) = do
    let a' = unbox a :: Maybe Bool
    let b' = unbox b :: Maybe Bool
    case (a',b') of
        (Nothing, _)     -> booleanError params
        (_, Nothing)     -> booleanError params
        (Just i, Just j) -> Return $ box (i && j)
arbre_and params = typeError params -- FIXME

arbre_or :: Native
arbre_or params@(a:b:[]) = do
    let a' = unbox a :: Maybe Bool
    let b' = unbox b :: Maybe Bool
    case (a',b') of
        (Nothing, _)     -> booleanError params
        (_, Nothing)     -> booleanError params
        (Just i, Just j) -> Return $ box (i || j)
arbre_or params = typeError params -- FIXME

arbre_not :: Native
arbre_not params@(a:[]) = do
    let a' = unbox a :: Maybe Bool
    case a' of
        Nothing -> booleanError params
        Just i  -> Return $ box (not i)
arbre_not params = typeError params -- FIXME

string_append :: Native
string_append params@(a:b:[]) = do
    let a' = unbox a :: Maybe String
    let b' = unbox b :: Maybe String
    case (a',b') of
        (Nothing, _)     -> stringError params
        (_, Nothing)     -> stringError params
        (Just i, Just j) -> Return $ box (i ++ j)
string_append params = typeError params -- FIXME

string_eq :: Native
string_eq params@(a:b:[]) = do
    let x = unbox a :: Maybe String
    let y = unbox b :: Maybe String
    case (x,y) of
        (Nothing,_) -> stringError params
        (_,Nothing) -> stringError params
        (Just i, Just j) -> Return $ box (i==j)
string_eq params = stringError params

class Boxable a where
  box :: a -> Value
  unbox :: Value -> Maybe a

instance Boxable String where
  box a = ObjectValue $ Object (LiteralState $ StringLit a) stringdef
  unbox exp =
    case exp of
      (ObjectValue (Object (LiteralState (StringLit a)) _)) -> Just a
      otherwise -> Nothing

instance Boxable Integer where
  box a = ObjectValue $ Object (LiteralState $ IntegerLit a) numdef
  unbox exp =
    case exp of
      (ObjectValue (Object (LiteralState (IntegerLit a)) _)) -> Just a
      otherwise -> Nothing

instance Boxable Bool where
  box a = ObjectValue $ Object (LiteralState $ BooleanLit a) booldef
  unbox exp =
    case exp of
      (ObjectValue (Object (LiteralState (BooleanLit a)) _)) -> Just a
      otherwise -> Nothing

instance Boxable Float where
  box a = ObjectValue $ Object (LiteralState $ FloatLit a) floatdef
  unbox exp =
    case exp of
      (ObjectValue (Object (LiteralState (FloatLit a)) _)) -> Just a
      otherwise -> Nothing

boxString :: String -> Value
boxString s = box s

unboxString :: Value -> Maybe String
unboxString exp = unbox exp :: Maybe String

integerError :: [Value] -> Expression
integerError params = fl $ "Type error, not integer literals" ++ (show params)

floatError :: [Value] -> Expression
floatError params = fl $ "Type error, not float literals" ++ (show params)

unboxFloat :: Value -> Maybe Float
unboxFloat (ObjectValue (Object (LiteralState (FloatLit i)) _)) = Just i
unboxFloat _ = Nothing

booleanError :: [Value] -> Expression
booleanError params = fl $ "Type error, not boolean literals" ++ (show params)

stringError :: [Value] -> Expression
stringError params = fl $ "Type error, not string literals" ++ (show params)

typeError :: [Value] -> Expression
typeError params = fl $ "General type error: " ++ (show params)
