-- Questao 1!
custoLocacao :: Int -> (Float,Float)
custoLocacao numDvds = (total, media)
	where
		n = fromIntegral numDvds
		total | n <= 6 = 4* n
			  | n <=12 = 3.5 * 6 + (n-6) *3
			  | n <=20 = 3.5 * 12 + (n-12) *3
			  | otherwise = 2.5 * n
		media = total / n

--Questao 2!
intervalo :: Integer->Integer->[Integer]
intervalo a b | a > b = []
			  | otherwise = a : intervalo (a+1) b

--Questao 3!
type Nome = String
type Telefone = String
type BancoDados = [(Nome,Telefone)]

agenda :: BancoDados
agenda =
		[("Ana Maria","3551-6228"),
		 ("Paulo","3551-0228"),
		 ("Rodrigo","3552-2322"),
		 ("Felipe","3551-5677"),
		 ("Marina","3551-9999")
		]
existeContato :: Nome->BancoDados->Bool
existeContato _ [] = False
existeContato nome ((pessoa,telefone):resto) = nome == pessoa || existeContato nome resto

existeContato2 :: Nome->BancoDados->Bool
existeContato2 _ [] = False
existeContato2 nome ((pessoa,telefone):resto) | nome == pessoa = True
											  | otherwise = existeContato2 nome resto

--Questao 4!
distanciaTotal [] = 0
distanciaTotal ((_,_,_,d):resto) = d + distanciaTotal resto

--Questao 5!
type Time = String
type Gols = Int
type Jogo = ((Time,Gols),(Time,Gols))
type Campeonato = [Jogo]
mesmoTime :: Campeonato->Bool
mesmoTime [] = False
mesmoTime (((t1,_),(t2,_)):resto) = t1 == t2 || mesmoTime resto
