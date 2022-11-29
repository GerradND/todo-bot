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
import           Database.Persist.Types
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Time.Clock(UTCTime)
import Data.List    
import Debug.Trace
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Int (Int64)
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Todo
        title String
        description String
        deadline_date UTCTime
        created_date UTCTime
        status Int
        server_id String
        created_by String
        UniqueTitle title
        
        deriving Show

    UserD
        name Text
        message Text
        deriving Show
|]

-- connStr = "host=localhost dbname=postgres user=postgres password=postgres port=5432"
