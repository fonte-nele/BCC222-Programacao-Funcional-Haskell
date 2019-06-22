module Naturais where

data Nat = Zero | Succ Nat
	deriving (Show)
	
um, dois, tres, quatro :: Nat
um = Succ Zero
dois = Succ um
tres = Succ dois
quatro = Succ tres

nat2integer  :: Nat -> Integer
nat2integer Zero = 0
nat2integer (Succ p) = 1 + nat2integer p

integer2nat :: Integer -> Nat
integer2nat 0 = Zero
integer2nat i | i >0 = Succ (integer2nat(i-1))

natLt :: Nat->Nat->Bool
natLt _           Zero      = False
natLt Zero      (Succ _) = True
natLt (Succ p) (Succ q) = natLt p q

natAdd :: Nat->Nat->Nat
natAdd m Zero = m
natAdd m (Succ n) = natAdd (Succ m) n

natSub :: Nat ->Nat->Nat
natSub m Zero = m
natSub (Succ m) (Succ n) = natSub m n

natDiv :: Nat->Nat->Nat
natDiv m n | natLt m n = Zero
				 | otherwise = Succ (natDiv(natSub m n) n)