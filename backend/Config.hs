module Config
    ( Config(..)
    , DbConfig(..)
    , fromEnvironment
    , makeDbPool
    ) where

import Text.Regex.Posix
import Control.Applicative
import Data.Maybe
import Database.Persist.Sqlite
import Data.String.Conversions
import Control.Monad.Logger (runStderrLoggingT)
import Data.Char (toUpper)

data Protocol
    = Sqlite
    | Postgres
    deriving (Show, Read, Eq)

data DbConfig = DbConfig
    { getProtocol :: Protocol
    , getUser :: String
    , getPassword :: String
    , getHost :: String
    , getPort :: Int
    , getDatabase :: String
    }

data Config = Config 
    { getServerPort :: Int
    , getDbConfig :: DbConfig
    }

defaultDb = DbConfig 
    { getProtocol = Sqlite
    , getUser     = ""
    , getPassword = ""
    , getHost     = ""
    , getPort     = 0
    , getDatabase = ":memory:"
    }

fromEnvironment :: [(String, String)] -> Config
fromEnvironment env = 
    let
        port = read $ fromMaybe "8080" $ lookup "PORT" env

        dbConfig = fromMaybe defaultDb
                    $ parseDbUrl
                    $ fromMaybe ""
                    $ lookup "DATABASE_URL" env
    in
        Config 
            { getServerPort = port
            , getDbConfig = dbConfig
            }

makeDbPool :: Config -> IO ConnectionPool
makeDbPool cfg =
    let
        dbConfig = getDbConfig cfg

        pool = case getProtocol dbConfig of
            Sqlite -> createSqlitePool (cs $ getDatabase dbConfig) 5
            Postgres -> createPostgresqlPool (toConnectionString dbConfig) 5 
    in
        runStderrLoggingT pool

parseDbUrl :: String -> Maybe DbConfig
parseDbUrl url =
    let 
        capitalize (c:s) = toUpper c:s
        capitalize s = s
        
        sqliteResult = url =~ ("^sqlite://(.+)$" :: String) :: (String, String, String, [String])

        sqliteConfig = case sqliteResult of
            (_, _, _, filename:_) ->
                Just DbConfig 
                    { getProtocol = Sqlite
                    , getUser     = ""
                    , getPassword = ""
                    , getHost     = ""
                    , getPort     = 0
                    , getDatabase = filename
                    }

            _ -> Nothing

        result = url =~ ("^([a-z]+)://([^:]+):([^@]+)@([^:]+):([^/]+)/([^/]+)$" :: String) :: (String, String, String, [String])
        
        dbConfig = case result of
            (_, _, _, protocol:user:password:host:port:database:_) ->
                Just DbConfig 
                    { getProtocol = read $ capitalize protocol
                    , getUser     = user
                    , getPassword = password
                    , getHost     = host
                    , getPort     = read port
                    , getDatabase = database
                    }

            _ -> Nothing
        in
            sqliteConfig <|> dbConfig

toConnectionString :: DbConfig -> ConnectionString
toConnectionString DbConfig
    { getProtocol = _
    , getUser     = user
    , getPassword = pass
    , getHost     = host
    , getPort     = port
    , getDatabase = database
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
        unwords $ zipWith (++) fields values
    
