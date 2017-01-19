module DbUrl where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)

import DbConfig 

parseDbUrl :: String -> Either ParseError DbConfig
parseDbUrl = parse dbUrl "Database URL"

dbUrl :: ParsecT String () Identity DbConfig
dbUrl = try sqliteUrl <|> postgresqlUrl

sqliteUrl :: ParsecT String () Identity DbConfig
sqliteUrl = do
    _ <- string "sqlite://" 
    filename <- many anyChar 
    return $ SqliteConfig filename

postgresqlUrl :: ParsecT String () Identity DbConfig
postgresqlUrl = do
    _ <- string "postgres://" 
    user <- many (noneOf ":")  
    _ <- char ':'
    password <- many (noneOf "@")
    _ <- char '@'
    host <- many (noneOf ":")
    _ <- char ':'
    port <- many digit
    _ <- char '/'
    database <- many anyChar
    return $ PostgresqlConfig user password host (read port) database

