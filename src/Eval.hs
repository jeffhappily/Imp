{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Eval where

import Data.Kind ( Type )
import Expr

-- | Evaluates arithmetic expression given a state
aeval :: State -> AExp -> Val
aeval st a =
  case a of
    ANum n -> n
    AId x -> st x
    APlus a1 a2 -> aeval st a1 + aeval st a2
    AMinus a1 a2  -> aeval st a1 - aeval st a2
    AMult a1 a2 -> aeval st a1 * aeval st a2

data AEValR :: State -> AExp -> Val -> Type where
  ANum :: AEValR s 

-- | Evaulates boolean expression given a state
beval :: State -> BExp -> Bool
beval st b =
  case b of
    BTrue       -> True
    BFalse      -> False
    BEq a1 a2   -> aeval st a1 == aeval st a2
    BLe a1 a2   -> aeval st a1 <= aeval st a2
    BNot b1     -> not (beval st b1)
    BAnd b1 b2  -> beval st b1 && beval st b2

-- | Command evaluation rules
data CEval :: Com -> State -> Result -> State -> Type where
  ESkip :: CEval CSkip s SContinue s
  EBreak :: CEval CBreak s SBreak s
  EAss :: aeval st a1 = n -> CEval
