module DbConfig where

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

