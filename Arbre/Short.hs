module Arbre.Short
(
    num,
    numv,
    float,
    bool,
    string,
    block,
    blockv,
    modul,
    self,
    selff,
    local,
    env,
    dyn,
    true,
    false,
    truev,
    falsev,
    prnt,
--    define,
--    set,
--    combine,
--    superdefine,
--    superset,
--    stdin,
  def
)
where

import Arbre.Expressions
import Arbre.Box
import Arbre.Native
import Arbre.Objects

def :: String -> [String] -> Expression -> Def
def name params body = Def name (blockv params body)

num :: Integer -> Expression
num i = do
    let lit = IntegerLit i
    let state = LiteralState lit
    let obj = Object state numdef
    Return $ ObjectValue obj

numv :: Integer -> Value
numv i = do
    let lit = IntegerLit i
    let state = LiteralState lit
    let obj = Object state numdef
    ObjectValue obj

float :: Float -> Expression
float i = do
    let lit = FloatLit i
    let state = LiteralState lit
    let obj = Object state numdef
    Return $ ObjectValue obj

bool :: Bool -> Expression
bool i = do
    let lit = BooleanLit i
    let state = LiteralState lit
    let obj = Object state numdef
    Return $ ObjectValue obj

boolv :: Bool -> Value
boolv i = do
    let lit = BooleanLit i
    let state = LiteralState lit
    let obj = Object state numdef
    ObjectValue obj

string :: String -> Expression
string i = do
    let lit = StringLit i
    let state = LiteralState lit
    let obj = Object state stringdef
    Return $ ObjectValue obj

true :: Expression
true = bool True

false :: Expression
false = bool False

truev :: Value
truev = boolv True

falsev :: Value
falsev = boolv False

block :: [String] -> Expression -> Expression
block name call = Return $ BlockValue $ Block name call

blockv :: [String] -> Expression -> Value
blockv name call = BlockValue $ Block name call

modul :: [Def] -> Object
modul defs = Module $ ObjectDef defs

self :: String -> Expression
self name = Symref $ Symbol Self name

selff :: Expression
selff = self ""

local :: String -> Expression
local name = Symref $ Symbol Local name

env :: String -> Expression
env name = Symref $ Symbol Lex name

dyn :: String -> Expression
dyn name = Symref $ Symbol Dyn name

prnt :: Expression -> Expression
prnt exp = exp

--define :: String -> Expression -> Expression
--define sym value = Mutation Define (Symbol sym) value

--set :: String -> Expression -> Expression
--set sym value = Mutation Set (Symbol sym) value

--superdefine :: [(String, Integer)] -> Expression
--superdefine defs = combine (map definePair defs)

--superset :: [(String, Expression)] -> Expression
--superset defs = combine (map setPair defs)

--definePair :: (String, Integer) -> Expression
--definePair (k,v) = define k (num v)

--setPair :: (String, Expression) -> Expression
--setPair (k,v) = set k v

--stdin :: Expression -> Expression
--stdin exp = Receiver Stdin $ block ["input"] exp
