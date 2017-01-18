module Config where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Data.String.Conversions
import Control.Monad.Logger (runStderrLoggingT)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)

data DbConfig 
    = PostgresqlConfig
        { postgresqlUser :: String
        , postgresqlPassword :: String
        , postgresqlHost :: String
        , postgresqlPort :: Int
        , postgresqlDatabase :: String
        }
    | SqliteConfig String 
    deriving (Show)

data Config = Config 
    { configServerPort :: Int
    , configDbConfig :: DbConfig
    }
    deriving (Show)

defaultDb :: DbConfig
defaultDb = SqliteConfig ":memory:"

fromEnvironment :: [(String, String)] -> Config
fromEnvironment env = 
    let
        port = read $ fromMaybe "8080" $ lookup "PORT" env

        dbConfig = either (const defaultDb) id
                    $ parse parseDbUrl "ParseUrl"
                    $ fromMaybe ""
                    $ lookup "DATABASE_URL" env
    in
        Config 
            { configServerPort = port
            , configDbConfig = dbConfig
            }

makeDbPool :: Config -> IO ConnectionPool
makeDbPool cfg =
    let
        dbConfig = configDbConfig cfg

        pool = case dbConfig of
            SqliteConfig filename -> createSqlitePool (cs filename) 5
            PostgresqlConfig {} -> createPostgresqlPool (toConnectionString dbConfig) 5 
    in
        runStderrLoggingT pool


parseDbUrl :: ParsecT String () Identity DbConfig
parseDbUrl = try sqliteUrl <|> postgresqlUrl

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

toConnectionString :: DbConfig -> ConnectionString
toConnectionString PostgresqlConfig
    { postgresqlUser     = user
    , postgresqlPassword = pass
    , postgresqlHost     = host
    , postgresqlPort     = port
    , postgresqlDatabase = database
    }
    = 
    let
        fields = [ "host="
                 , "port="
                 , "user="
                 , "password="
                 , "dbname="
                 ]
        values = [ host
                 , show port
                 , user
                 , pass
                 , database
                 ]
    in
        BS.pack $ unwords $ zipWith (++) fields values
    
