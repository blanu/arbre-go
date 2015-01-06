{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Compile
(
  prepare,
  compile
)
where

import Data.Typeable
import Data.Data
import qualified Data.Map as M
import Data.Either
import qualified Debug.Trace as T
import qualified Data.Bimap as Bi
import qualified Data.List as L
import System.Directory

import Arbre.Expressions
import Arbre.Path
import Arbre.Native
import Arbre.View
import Arbre.Mutation
import Arbre.Context as C
import Arbre.Event
import Arbre.Receiver

prepare :: IO()
prepare = do
  copyLib "arbre.go"
  copyLib "expression.go"
  copyLib "native.go"

copyLib :: String -> IO()
copyLib name = copyFile ("lib/"++name) ("src/arbre/"++name)

compile :: String -> Object -> IO()
compile filename modl = do
  let s = compileMainObject filename modl
  putStrLn s
  writeFile ("src/arbre/test_"++filename++".go") s

compileMainObject :: String -> Object -> String
compileMainObject filename (Object state def) =
  "package main\n\n" ++ compileMainState state ++ compileMainDefs filename def
compileMainObject filename (Module def) =
  "package main\n\nimport \"testing\"\n\n" ++ genClass filename ++ compileMainDefs filename def ++ "\n" ++ genMain filename

compileMainState :: State -> String
compileMainState state = "state"

compileMainDefs :: String -> ObjectDef -> String
compileMainDefs name (ObjectDef defs) = foldl (++) "" $ map (compileDef $ "Test"++name) defs

compileDef :: String -> Def -> String
compileDef clsname (Def sym (BlockValue block)) = compileBlockDef clsname sym block
compileDef clsname (Def sym val) = "func (self " ++ clsname ++ ") " ++ sym ++ "() Object {return " ++ (compileValue val) ++ "}\n"

compileValue :: Value -> String
compileValue (ObjectValue (Object (LiteralState lit) _)) = compileLiteral lit
compileValue (BlockValue block) = compileBlockValue block
compileValue val = show val

compileBlockDef :: String -> String -> Block -> String
compileBlockDef clsname "main" (Block args body) = "func (self " ++ clsname ++ ") main() " ++ compileMainBody body
compileBlockDef clsname sym (Block args body) = "func (self " ++ clsname ++ ") " ++ sym ++ compileArgs args ++ " Object " ++ compileBody body

compileArgs :: [String] -> String
compileArgs args = "(" ++ (L.intercalate ", " $ map compileArg args) ++ ")"

compileArg :: String -> String
compileArg name = name ++ " " ++ "Object"

compileMainBody :: Expression -> String
compileMainBody body = "{\n  print(" ++ compileExpression body ++ ".String())\n}\n\n"

compileBody :: Expression -> String
compileBody body = "{\n" ++ "  return " ++ compileExpression body ++ "\n}\n\n"

compileInlineBody :: Expression -> String
compileInlineBody body = "{ " ++ "return " ++ compileExpression body ++ " }"

compileExpression :: Expression -> String
compileExpression (Call target method args) = compileTarget target ++ "." ++ method ++ "(" ++ compileCallArgs args ++ ")"
compileExpression (Symref symbol) = compileSymbol symbol
compileExpression (Return val) = compileValue val
compileExpression exp = show exp

compileTarget :: Expression -> String
compileTarget (Symref symbol) = compileSymbol symbol
compileTarget (Return val) = compileValue val
compileTarget target = show target

compileCallArgs :: [Expression] -> String
compileCallArgs args = L.intercalate ", " $ map compileExpression args

compileSymbol :: Symbol -> String
compileSymbol (Symbol Local sym) = sym
compileSymbol (Symbol Lex sym) = sym
compileSymbol (Symbol Dyn sym) = sym
compileSymbol (Symbol Self "") = "self"
compileSymbol (Symbol Self sym) = "self." ++ sym ++ "()"
compileSymbol symbol@(Symbol sel sym) = show symbol

genClass :: String -> String
genClass name = "type Test" ++ name ++ " struct {}\n"

genMain :: String -> String
genMain name = "func Test" ++ name ++ "Main(t *testing.T) {\n  var obj=Test"++name++"{}\n  obj.main()\n}\n"

compileLiteral :: Literal -> String
compileLiteral (StringLit lit) = "Bytestring{inner: []byte(\"" ++ lit ++ "\")}"
compileLiteral (IntegerLit lit) = "Number{inner: " ++ show lit ++"}"
compileLiteral (FloatLit lit) = "Number{inner: " ++ show lit ++"}"
compileLiteral (BooleanLit True) = "Boolean{inner: true}"
compileLiteral (BooleanLit False) = "Boolean{inner: false}"
compileLiteral lit = "literal"

compileBlockValue :: Block -> String
compileBlockValue (Block args exp) = "func" ++ compileArgs args ++ compileInlineBody exp
