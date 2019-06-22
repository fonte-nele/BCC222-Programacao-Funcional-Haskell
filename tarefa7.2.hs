numRaizes :: (Num a, Ord a)=> a->a->a->String
numRaizes a b c | (valor >0) = "02 raizes"
                | (valor==0) = "01 raiz"
                | (valor <0) = "Nenhuma raiz"
                where
                    valor = b^2 - 4*a*c
