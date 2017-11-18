{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import           Data.Text              (pack)
import           Tasks.GADT_1.GADTExpr
import           Text.Parsec.Char       (char, digit, space, string)
import           Text.Parsec.Combinator (between, many1)
import           Text.Parsec.Language   (haskellDef)
import           Text.Parsec.Prim       (many, parseTest, try, (<|>))
import           Text.Parsec.Text       (Parser)
import           Text.Parsec.Token

iLitP :: Parser (Lit Int)
iLitP = try $ ILit <$> read <$> (spacedP (many1 digit))

bLitP :: Parser (Lit Bool)
bLitP =  try $ BLit <$> (\b -> if (b == 'T') then True else False) <$> (spacedP (char 'T' <|> char 'F'))

iiLitP :: Parser (Expr Int)
iiLitP = Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = Lit <$> bLitP

addP :: Parser (Expr Int)
addP = try $ Add <$> (spacedP  ((bracketP parse) <|> iiLitP)) <*> (char '+' *> (spacedP parse))

leqP :: Parser (Expr Bool)
leqP = Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = try  $ And<$> (spacedP  ((bracketP parse) <|> leqP <|> bbLitP )) <*>  (string "&&" *> (spacedP parse))

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space

bracketP :: Parser a -> Parser a
bracketP = try <$> between (char '(') (char ')')

class MyParse a where
  parse :: Parser (Expr a)

instance MyParse Int where
  parse = (try $ (spacedP addP)) <|> (try $ (spacedP (bracketP parse))) <|> iiLitP
instance MyParse Bool where
  parse = (try $ (spacedP andP)) <|> (try $ (spacedP leqP)) <|> (try $ (spacedP (bracketP parse))) <|> bbLitP 

