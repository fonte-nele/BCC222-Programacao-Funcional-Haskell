mdc :: Integral a=> a->a->a
mdc a b | b==0 = a
        | b>0  = mdc b (mod a b)
        | b<0  = mdc a (-b)
