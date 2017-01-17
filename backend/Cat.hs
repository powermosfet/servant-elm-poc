{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Cat 
  ( Cat
  , CatId
  , migrateCats
  ) where

-- import Database.Persist
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
