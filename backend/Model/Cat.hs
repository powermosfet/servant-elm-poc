{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.Cat where

import Database.Persist.Sql
import Database.Persist.TH
import Data.Aeson

share [mkPersist sqlSettings, mkMigrate "migrateCats"] [persistLowerCase|
Cat
    name String
    UniqueName name
    deriving Eq Read Show
|]

instance FromJSON Cat where
  parseJSON = withObject "Cat" $ \ v ->
    Cat <$> v .: "name"

instance ToJSON Cat where
  toJSON (Cat name) =
    object [ "name" .= name ]

getCats :: ConnectionPool -> IO [Cat]
getCats pool = flip runSqlPersistMPool pool $ do
    entUsers <- selectList [] []
    return $ entityVal <$> entUsers

postCat :: ConnectionPool -> Cat -> IO (Maybe (Key Cat))
postCat pool cat = flip runSqlPersistMPool pool $ do
    exists <- selectFirst [CatName ==. catName cat] []
    case exists of
        Nothing -> Just <$> insert cat
        Just _ -> return Nothing
