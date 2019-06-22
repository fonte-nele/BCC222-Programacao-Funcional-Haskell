module BinTree where

data BinTree a = V | N a (BinTree a) (BinTree a)
               deriving (Read, Show)

vazia :: BinTree a -> Bool
vazia V = True
vazia _ = False

insere :: (Ord a) => a -> BinTree a -> BinTree a
insere x V = N x V V
insere x (N y esq dir) | x <= y    = N y (insere x esq) dir
                       | otherwise = N y esq (insere x dir)

elemento :: (Eq a) => a -> BinTree a -> Bool
elemento _ V = False
elemento x (N y esq dir) = x == y ||
                           elemento x esq ||
                           elemento y dir


instance Eq a => Eq (BinTree a) where
  V == V = True
  N x1 esq1 dir1 == N x2 esq2 dir2 = x1 == x2 &&
                                     esq1 == esq2 &&
                                     dir1 == dir2
  _ == _ = False


instance Ord a => Ord (BinTree a) where
  compare V V = EQ
  compare V _ = LT
  compare (N _ _ _) V = GT
  compare (N x1 esq1 dir1) (N x2 esq2 dir2) =
    case compare x1 x2 of
      EQ -> case compare esq1 esq2 of
              EQ -> compare dir1 dir2
              k -> k
      k -> k

instance Functor BinTree where
  fmap _ V = V
  fmap f (N x esq dir) = N (f x) (fmap f esq) (fmap f dir)


t1 :: BinTree Integer
t1 = insere 8 $ insere 5 $ insere (-2) $ insere 17 $ insere 11 $ insere 33 $ insere 9 V
