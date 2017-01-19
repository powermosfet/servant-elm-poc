module Api where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Network.Wai

import Model.Cat
import Config

type MyAPI = "cats" :> Get '[JSON] [Cat]
        :<|> "cats" :> ReqBody '[JSON] Cat :> Post '[JSON] (Maybe (Key Cat))
        :<|> Raw

myAPI :: Proxy MyAPI
myAPI =
    Proxy

server :: ConnectionPool -> Server MyAPI
server pool = getCatsH
         :<|> postCatH
         :<|> serveDirectory "static/"
 
    where
      getCatsH = liftIO (getCats pool)
      postCatH cat = liftIO (postCat pool cat)

mkApp :: Config -> IO Application
mkApp cfg = do
    pool <- makeDbPool cfg
    runSqlPool (runMigration migrateCats) pool
    return $ app pool

app :: ConnectionPool -> Application
app pool = serve myAPI $ server pool
