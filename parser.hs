module Parser where

-- tipo dos parsers
--
-- um parser é uma função que converte uma string para um valor de um tipo
-- apropriado ao contexto

-- o tipo será polimórfico para generalizar o tipo do valor extraído da
-- string

-- porém nem toda a string será consumida sempre, podendo ser utilizada
-- apenas os seus caracteres iniciais; assim o resultado da função deve
-- incluir também os caracteres da string que não foram consumidos pelo
-- parser

-- o parser pode falhar (nenhum valor pode ser obtido da string) ou suceder
-- (um ou mais valores podem ser obtidos); assim o resultado final será uma
-- lista dos valores obtidos

newtype Parser a = Parser (String -> [(a,String)])

runParser :: Parser a -> String -> [(a,String)]
runParser (Parser f) s = f s

failure :: Parser a
failure = Parser $ \_ -> []

item :: Parser Char
item = Parser $ \s -> case s of
                       "" -> []
                       x:xs -> [(x, xs)]

instance Functor Parser where
  fmap f (Parser fun) =
    Parser $ \s -> [ (f x, s1) | (x,s1) <- fun s ]

instance Applicative Parser where
  pure x = Parser $ \s -> [(x,s)]

  Parser fun1 <*> Parser fun2 =
    Parser $ \s -> [ (f x,s2) | (f,s1) <- fun1 s, (x,s2) <- fun2 s1 ]


-- Exemplo:
-- *Parser Data.Char> runParser (fmap max item <*> item) "abc"
-- [('b',"c")]
