area :: (Num a,Floating a)=> a->a->a->a
area x y z = cosseno * h / 2
            where
                cosseno = (y^2 + z^2 - x^2) / (2*y*z)
                seno = sqrt(1- cosseno^2)
                h = y * seno
