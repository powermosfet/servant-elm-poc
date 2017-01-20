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

share [mkPersist sqlSettings, mkMigrate "migrateCats"] [persistLowerCase|
Cat json
    name String
    UniqueName name
    deriving Show
|]

getCats :: ConnectionPool -> IO [Entity Cat]
getCats pool = flip runSqlPersistMPool pool $ selectList [] []

postCat :: ConnectionPool -> Cat -> IO (Maybe (Key Cat))
postCat pool cat = flip runSqlPersistMPool pool $ do
    exists <- selectFirst [CatName ==. catName cat] []
    case exists of
        Nothing -> Just <$> insert cat
        Just _ -> return Nothing
