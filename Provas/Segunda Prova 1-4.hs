-- q1
selEnquanto :: (a -> Bool) -> [a] -> [a]
selEnquanto _ [] = []
selEnquanto test (x:xs) | test x    = x : selEnquanto test xs
                        | otherwise = []

-- q2
type Nome     = String
type Telefone = String
type Agenda   = [ (Nome, Telefone) ]

minhaAgenda :: Agenda
minhaAgenda =
  [ ("Ana Maria" , "3551-6228"),
    ("Paulo"     , "3551-0222"),
    ("Rodrigo"   , "3552-2322"),
    ("Felipe"    , "3551-5677"),
    ("Felipe"    , "9999-8888"),
    ("Marina"    , "3551-9999"),
    ("Felipe"    , "9876-5432")
  ]

achaContatos :: Nome -> Agenda -> [Telefone]
achaContatos nome agenda =
  map snd (filter ((nome==) . fst) agenda)

-- q3
type Time       = String
type Gols       = Int
type Jogo       = ( (Time, Gols), (Time, Gols) )
type Campeonato = [ Jogo ]
type Pontos     = Int
type Tabela     = [ (Time, Pontos) ]

campMineiro :: Campeonato
campMineiro = [ (("cruzeiro"  ,0), ("atletico",0))
              , (("uberlandia",5), ("america", 1))
              , (("atletico"  ,1), ("america", 2))
              , (("uberlandia",2), ("cruzeiro",1))
              , (("america"   ,4), ("cruzeiro",3))
              , (("urt"       ,0), ("atletico",1))
              , (("urt"       ,2), ("america", 2))
              ]

poeNaTabela :: Tabela -> (Time,Pontos) -> Tabela
poeNaTabela [] x = [x]
poeNaTabela ((t,p):resto) (time,pontos)
  | t == time = (t,p+pontos) : resto
  | otherwise = (t,p) : poeNaTabela resto (time,pontos)

computaJogo :: Tabela -> Jogo -> Tabela
computaJogo tabela ((time1,gols1),(time2,gols2))
  | gols1 > gols2 = poeNaTabela
                      (poeNaTabela tabela (time1,3))
                      (time2,0)
  | gols1 < gols2 = poeNaTabela
                      (poeNaTabela tabela (time1,0))
                      (time2,3)
  | otherwise = poeNaTabela
                  (poeNaTabela tabela (time1,1))
                  (time2,1)

pontos :: Campeonato -> Tabela
pontos campeonato =
  foldl computaJogo [] campeonato


-- q4
-- anulada
