module Expr where

-- import qualified Data.Map as Map

-- | Variable name
type Name = String

-- | Variable Value
type Val = Integer

-- | Arithmetic expressions
data AExp
  = ANum Integer -- was Nat in LF
  | AId Name -- variable
  | APlus AExp AExp
  | AMinus AExp AExp
  | AMult AExp AExp

-- | Boolean expressions
data BExp
  = BTrue
  | BFalse
  | BEq AExp AExp
  | BLe AExp AExp
  | BNot BExp
  | BAnd BExp BExp

-- | Commands, like statements
data Com
  = CSkip
  | CBreak
  | CAss Name AExp
  | CSeq Com Com
  | CIf BExp Com Com
  | CWhile BExp Com
  | CFor Com BExp Com Com

-- type State = Map.Map Name Val
type State = Name -> Val
data Result = SContinue | SBreak
