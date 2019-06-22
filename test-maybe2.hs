data Sexo = M | F
data Pessoa = P String Int (Maybe Sexo)

p1 = P "joao" 23 (Just M)
p2 = P "marina" 44 (Just F)
p3 = P "thiago" 20 Nothing
p4 = P "nina" 10 (Just F)
