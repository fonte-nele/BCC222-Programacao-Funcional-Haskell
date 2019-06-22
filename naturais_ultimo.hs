module Nat where

data Nat = Zero | Succ Nat

instance Eq Nat where
  Zero == Zero      = True
  Succ p == Succ q  = p == q
  _ == _            = False


instance Ord Nat where
  compare Zero     Zero     = EQ
  compare Zero     (Succ _) = LT
  compare (Succ _) Zero     = GT
  compare (Succ p) (Succ q) = compare p q


instance Num Nat where
  p + Zero   = p
  p + Succ q = Succ (p + q)

  p      - Zero   = p
  Succ p - Succ q = p - q
  _      - _      = error "Nat cannot be negative"

  _ * Zero = Zero
  p * Succ q = p + p * q

  negate _ = error "Nat cannot be negated"

  abs n = n

  signum Zero     = Zero
  signum (Succ _) = Succ Zero  -- 1

  fromInteger 0 = Zero
  fromInteger i | i > 0 = Succ (fromInteger (pred i))
                | otherwise = error "Negative Integer"

instance Enum Nat where
  toEnum 0             = Zero
  toEnum i | i > 0     = Succ (toEnum (pred i))
           | otherwise = error "Negative Int"

  fromEnum Zero = 0
  fromEnum (Succ p) = 1 + fromEnum p

instance Show Nat where
  show n = '#' : show (natToInteger n)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ p) = 1 + natToInteger p

instance Read Nat where
  -- readsPrec :: Int -> ReadS Nat
  -- readsPrec :: Int -> String -> [(Nat,String)]

  readsPrec p ('#' : str) = map (\(i,s) -> (fromInteger i,s)) (readsPrec p str)
  readsPrec _ _           = error "no parse"
