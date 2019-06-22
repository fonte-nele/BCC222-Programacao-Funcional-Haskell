module Parser (parseExp) where

import Control.Applicative (pure,(<*>),(<$>),(<$),(<*),(*>))
import Text.Parsec

import Exp

parseExp = pExp

pExp :: Parsec String () Exp
pExp = chainl1 pTerm (lexeme (Sum <$ char '+' <|> Sub <$ char '-'))

pTerm :: Parsec String () Exp
pTerm = chainl1 pFactor (lexeme (Mul <$ char '*' <|> Div <$ char '/'))

pFactor :: Parsec String () Exp
pFactor = pLet <|>
          Cte <$> lexeme pInteger <|>
          Var <$> lexeme pId <|>
          lexeme (char '(') *> pExp <* lexeme (char ')')

pLet :: Parsec String () Exp
pLet  = Let <$> (lexeme (string "let") *> lexeme pId) <*>
                (lexeme (char '=') *> lexeme pExp) <*>
                (lexeme (string "in") *> lexeme pExp)

pInteger :: Parsec String () Integer
pInteger = read <$> many1 digit

pId :: Parsec String () String
pId = (:) <$> letter <*> many (alphaNum <|> char '-')

lexeme :: Parsec String () a -> Parsec String () a
lexeme p = p <* spaces
