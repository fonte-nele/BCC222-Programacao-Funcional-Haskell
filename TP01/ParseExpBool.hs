module ParseExpBool where

import Text.Parsec
import ExpBool

type Parser = Parsec String () ExpBool

parseExpBool :: String -> Maybe ExpBool
parseExpBool str =
         case parse pExp "" str of
             Left _ -> Nothing
             Right e -> Just e

lexeme :: Parsec String () a -> Parsec String () a
lexeme p = do spaces
              x <- p
              spaces
              return x

pExp :: Parser
pExp = pDis

pAtm :: Parser
pAtm = try (lexeme (string "1") >> return (Cte True))
            <|>
            try (lexeme (string "0") >> return (Cte False))
            <|>
            try (fmap Var (lexeme (many1 letter)))
            <|>
            try (between (lexeme (string "(")) (lexeme(string ")")) pExp)
            <|>
            (lexeme (string "~") >> (fmap Neg pAtm))

pCon :: Parser
pCon = fmap (foldl1 Con) (many1 pAtm)

pDis :: Parser
pDis = fmap (foldl1 Dis) (sepBy1 pCon (lexeme (string "+")))
