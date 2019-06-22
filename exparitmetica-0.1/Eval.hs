module Eval where

import Exp

type Memory = [(String,Integer)]

data Reader r a = R (r -> a)

runReader (R f) r = f r

ask :: Reader r r
ask = R (\r -> r)

local :: (r -> r) -> Reader r a -> Reader r a
local f reader = R (\r -> runReader reader (f r))

instance Monad (Reader r) where
  -- COMPLETE ESTA DEFINIÇÃO DE INSTÂNCIA

eval :: Exp -> Reader Memory Integer
-- COMPLETE A DEFINIÇÃO DESTA FUNÇÃO
