{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module PostgresDB where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.Text               (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserD
    name Text
    message Text
    deriving Show
|]

-- generated datatype
-- data UserD = UserD
--   { userDName :: Text
--   , userDMessage :: Text
--   }

-- connStr = "host=localhost dbname=<DBNAME> user=<USER> password=<PASSWORD> port=<PORT>"

-- main :: IO ()
-- main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
--     flip runSqlPersistMPool pool $ do
--         runMigration migrateAll

--         johnId <- insert $ Person "John Doe" $ Just 35
--         janeId <- insert $ Person "Jane Doe" Nothing

--         insert $ BlogPost "My fr1st p0st" johnId
--         insert $ BlogPost "One more for good measure" johnId

--         oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
--         liftIO $ print (oneJohnPost :: [Entity BlogPost])

--         john <- get johnId
--         liftIO $ print (john :: Maybe Person)

--         delete janeId
--         deleteWhere [BlogPostAuthorId ==. johnId]