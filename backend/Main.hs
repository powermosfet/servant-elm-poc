{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

import Database.Persist
import Database.Persist.Sqlite
-- import Control.Concurrent.STM
import Control.Monad.IO.Class
-- import Data.Aeson
import Data.Proxy
-- import Data.Text
-- import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import Config
import Cat

getCats :: ConnectionPool -> IO [Cat]
getCats pool = flip runSqlPersistMPool pool $ do
    entUsers <- selectList [] []
    return $ entityVal <$> entUsers

type MyAPI =
    "cats" :> Get '[JSON] [Cat]
    :<|> Raw

myAPI :: Proxy MyAPI
myAPI =
    Proxy

server :: ConnectionPool -> Server MyAPI
server pool =
    getCatsH
    :<|> serveDirectory "static/"
 
    where
      getCatsH = liftIO (getCats pool)

mkApp :: Config -> IO Application
mkApp cfg = do
    pool <- makeDbPool cfg

    runSqlPool (runMigration migrateCats) pool
    return $ app pool

app :: ConnectionPool -> Application
app pool = serve myAPI $ server pool

-- main :: IO ()
-- main = do
--     hSetBuffering stdout LineBuffering
--     env <- getEnvironment
--     let port = maybe 8080 read $ lookup "PORT" env
--     cats <- staticCats
--     run port $ serve myAPI $ server cats

------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let config = fromEnvironment env 
    let port = getServerPort config
    run port =<< mkApp config
    --runSqlite ":memory:" $ do
    --    runMigration migrateAll
