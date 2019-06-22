maior :: Ord a =>a->a->a->a
maior x y z = if x>y && x>z
                then x
                else if y>z
                    then y
                    else z
