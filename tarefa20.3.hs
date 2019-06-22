data Exp = Cte Integer
         | Som Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         deriving (Read, Show)

avalia :: Exp -> Maybe Integer
avalia (Cte x) = Just x
avalia (Som a b) = avaliaAux (+) a b
avalia (Sub a b) = avaliaAux (-) a b
avalia (Mul a b) = avaliaAux (*) a b
avalia (Div a b) = case avalia a of
                     Nothing -> Nothing
                     Just x -> case avalia b of
                                 Nothing -> Nothing
                                 Just y | y == 0 -> Nothing
                                        | otherwise -> Just (div x y)

avaliaAux op a b = case avalia a of
                     Nothing -> Nothing
                     Just x -> case avalia b of
                                 Nothing -> Nothing
                                 Just y -> Just (op x y)

avalia' (Cte x) = return x
avalia' (Som a b) = avaliaAux' (+) a b
avalia' (Sub a b) = avaliaAux' (+) a b
avalia' (Mul a b) = avaliaAux' (+) a b
avalia' (Div a b) = avalia' a >>= \x ->
                    avalia' b >>= \y ->
                    if y == 0
                       then Nothing
                       else return (div x y)

avaliaAux' op a b = avalia' a >>= \x ->
                    avalia' b >>= \y ->
                    return (op x y)



-- mudan锟絘 de estado

newtype St s a = St (s -> (a,s))

runSt :: St s a -> s -> (a,s)
runSt (St f) s = f s

--avaliad :: Exp -> Int -> (Integer,Int)

avaliad :: Exp -> St Int Integer
avaliad (Cte i)   = St $ \s -> (i,s)
avaliad (Som a b) = avaliadAux (+) a b
avaliad (Sub a b) = avaliadAux (-) a b
avaliad (Mul a b) = avaliadAux (*) a b
avaliad (Div a b) = St $ \s -> let (x,s') = runSt (avaliad a) s
                                   (y,s'') = runSt (avaliad b) s'
                               in (div x y,s''+1)


avaliadAux op a b = St $ \s -> let (x,s') = runSt (avaliad a) s
                                   (y,s'') = runSt (avaliad b) s'
                               in (op x y,s'')


instance Monad (St s) where
  return x = St $ \s -> (x,s)

  m >>= h = St $ \s -> let (x,s1) = runSt m s
                       in runSt (h x) s1


avaliad' :: Exp -> St Int Integer
avaliad' (Cte i)   = return i
avaliad' (Som a b) = avaliadAux' (+) a b
avaliad' (Sub a b) = avaliadAux' (-) a b
avaliad' (Mul a b) = avaliadAux' (*) a b
avaliad' (Div a b) = do x <- avaliad' a
                        y <- avaliad' b
                        cont <- get
                        set (cont + 1)
                        return (div x y)

avaliadAux' op a b = do x <- avaliad' a
                        y <- avaliad' b
                        return (op x y)

get :: St s s
get = St $ \s -> (s,s)

set :: s -> St s ()
set x = St $ \_ -> ((),x)


-- avaliar uma expressao e produzir um "log" da avaliacao de todas as
-- subexpressoes

avaliatrace :: Exp -> (String,Integer)
avaliatrace (Cte i)   = (trace (Cte i) i, i)
avaliatrace (Som a b) = avaliatraceAux (Som a b) (+) a b
avaliatrace (Sub a b) = avaliatraceAux (Sub a b) (-) a b
avaliatrace (Mul a b) = avaliatraceAux (Mul a b) (*) a b
avaliatrace (Div a b) = avaliatraceAux (Div a b) div a b

avaliatraceAux e op a b = (s, z)
  where
    (sa,va) = avaliatrace a
    (sb,vb) = avaliatrace b
    z = op va vb
    s = sa ++ sb ++ trace e z


trace :: Exp -> Integer -> String
trace e v = show v ++ " <= " ++ show e ++ "\n"




newtype Out a = Out (String,a)

out :: String -> Out ()
out s = Out (s,())

instance Monad Out where
  return x = Out ("",x)

  m >>= p = let Out (s1,x1) = m
                Out (s2,x2) = p x1
            in Out (s1 ++ s2,x2)

avaliatrace' :: Exp -> Out Integer
avaliatrace' (Cte i)   = do out (trace (Cte i) i)
                            return i
avaliatrace' (Som a b) = avaliatrace'Aux (Som a b) (+) a b
avaliatrace' (Sub a b) = avaliatrace'Aux (Sub a b) (-) a b
avaliatrace' (Mul a b) = avaliatrace'Aux (Mul a b) (*) a b
avaliatrace' (Div a b) = avaliatrace'Aux (Div a b) div a b

avaliatrace'Aux e op a b = do va <- avaliatrace' a
                              vb <- avaliatrace' b
                              let z = op va vb
                              out (trace e z)
                              return z


data MaybeOut a = Fail | Success String a deriving (Show)

tell :: String -> MaybeOut ()
tell s = Success s ()

instance Monad MaybeOut where
  return x = Success "" x

  Fail          >>= _ = Fail
  Success s1 x1 >>= f = case f x1 of
                          Fail -> Fail
                          Success s2 x2 -> Success (s1 ++ s2) x2

avaliaMO :: Exp -> MaybeOut Integer
avaliaMO (Cte i)   = do tell (trace (Cte i) i)
                        return i
avaliaMO (Som a b) = avaliaMOAux (Som a b) (+) a b
avaliaMO (Sub a b) = avaliaMOAux (Sub a b) (-) a b
avaliaMO (Mul a b) = avaliaMOAux (Mul a b) (*) a b
avaliaMO (Div a b) = do va <- avaliaMO a
                        vb <- avaliaMO b
                        if vb == 0
                          then Fail
                          else do let z = div va vb
                                  tell (trace (Div a b) z)
                                  return z

avaliaMOAux e op a b = do va <- avaliaMO a
                          vb <- avaliaMO b
                          let z = op va vb
                          tell (trace e z)
                          return z
