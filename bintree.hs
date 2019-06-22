module BinTree where

data BinTreeInteger = V | N Integer BinTreeInteger BinTreeInteger
data BinTreeString  = V2 | N2 String  BinTreeString BinTreeString
data BinTreeDouble  = V3| N3 Double  BinTreeDouble BinTreeDouble
data BinTree a = Nulo | No a (BinTree a)
                                (BinTree a)
                                --deriving (Show)
instance Show a => Show (BinTree a)
  where
      show Nulo                  = "_"
      show (No x esq dir)  = "{" ++ show x ++ ":" ++
                                                  show esq ++ "|" ++
                                                  show dir ++ "}"

arv :: Num a => BinTree a
arv = (No 1
         (No 2
            (No 4 Nulo Nulo)(No 5 Nulo Nulo))
         (No 3
            (No 6 Nulo Nulo) Nulo))

bt1 :: BinTree Float
bt1 = No 10 Nulo Nulo

bt2 :: BinTree Int
bt2 = No 8
        (No 4 Nulo Nulo)
        (No 7 Nulo Nulo)

bt3 :: BinTreeString
bt3 = N2 "pedro"
        (N2 "ana"
           V2
           (N2 "carlos" V2 V2))
        (N2 "raquel" V2 V2)

btLength :: BinTree a -> Int
btLength Nulo = 0
btLength (No _ esq dir) = 1 + btLength esq  + btLength dir

btDepth :: BinTree a -> Int
btDepth Nulo = 0
btDepth (No _ esq dir) = 1 + max (btDepth esq) (btDepth dir)

btElem :: Eq a => a -> BinTree a -> Bool
btElem _ Nulo = False
btElem x (No y esq dir) = x == y ||
                    btElem x esq ||
                    btElem x dir

 -- Na funcao abaixo, elementos repetidos sao considerados para o lado esquerdo da arvore!
insert :: Ord a => a -> BinTree a -> BinTree a
insert x Nulo = (No x Nulo Nulo)
insert x (No y esq dir)
        | x <= y     = No y (insert x esq) dir
        | otherwise  = No y esq (insert x dir)

listToBinTree :: Ord a => [a] -> BinTree a
listToBinTree []     = Nulo
listToBinTree (x:xs) = listToBinTreeAux xs (No x Nulo Nulo)
                            where
                                listToBinTreeAux []  arvore= arvore
                                listToBinTreeAux (x:xs) arvore = listToBinTreeAux xs (insert x arvore)
{-Versao Romildo 18-07
listToBinTree []     = Nulo
listToBinTree (x:xs) = insert x (listToBinTree xs)
-}

binTreeToList :: BinTree a -> [a]
binTreeToList Nulo = []
binTreeToList (No x esq dir) = (binTreeToList esq) ++ [x] ++ (binTreeToList dir)

sort :: Ord a=> [a] -> [a]
sort [] = []
sort (x:xs) = binTreeToList(listToBinTree (x:xs))
--sort = binTreeToList.listToBinTree
