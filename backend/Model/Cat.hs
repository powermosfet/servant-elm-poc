{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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
    deriving Show
|]

instance FromJSON Cat where
    parseJSON = withObject "Cat" $ \ v ->
        Cat <$> v .: "name"

instance ToJSON (Entity Cat) where
    toJSON (Entity id (Cat name)) =
        object
          [ "data" .= object [ "name" .= name ]
          , "links" .= object
              [ "self" .= ("/cat/" ++ show id)
              ]
          ]

getCats :: ConnectionPool -> IO [Entity Cat]
getCats pool = flip runSqlPersistMPool pool $ selectList [] []

postCat :: ConnectionPool -> Cat -> IO (Maybe (Key Cat))
postCat pool cat = flip runSqlPersistMPool pool $ do
    exists <- selectFirst [CatName ==. catName cat] []
    case exists of
        Nothing -> Just <$> insert cat
        Just _ -> return Nothing
