data BinTree a = V
					  |  N a (BinTree a) (BinTree a)
					  deriving (Show)
					  
bt1 :: BinTree Float
bt1 = N 10 V V

bt2 :: BinTree Int
bt2 = N 8
			(N 4 V V)
			(N 7 V V)
			
bt3 :: BinTree String
bt3 = N "pedro"
			(N "ana"
			      V
				  (N "carlos" V V))
			(N "raquel" V V)
			
btLength :: BinTree a -> Int
btLenght V = 0
btLenght (N _ left right) = 1 + btLength left + btLenght right

btDepth :: BinTree a -> Int