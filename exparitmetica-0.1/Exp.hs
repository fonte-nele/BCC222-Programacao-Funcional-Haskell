module Exp where

data Exp = Cte Integer
         | Var String
         | Sum Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Let String Exp Exp
         deriving (Show)

