import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T


newtype Cat = Cat
    { name :: Text
    }
  deriving (Generic, Show)

instance FromJSON Cat
instance ToJSON Cat


staticCats :: IO (TVar [Cat])
staticCats =
    newTVarIO [ Cat {name="Felix"}
              , Cat {name="Bolivar"}
              ]

getCats :: MonadIO m => TVar [Cat] -> m [Cat]
getCats cats =
    liftIO $ readTVarIO cats

type MyAPI =
         Get Text
    :<|> "cats" :> Get [Cat]

myAPI :: Proxy MyAPI
myAPI =
    Proxy

server :: Text -> TVar [Cat] -> Server MyAPI
server home cats =
         return home
    :<|> getCats cats


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome to Haskell on Heroku" T.pack $
                 lookup "TUTORIAL_HOME" env
    cats <- staticCats
    run port $ serve myAPI $ server home cats
