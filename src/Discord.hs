{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Discord where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (useFullContext)
import qualified Calamity.Interactions as I
import Calamity.Metrics.Noop
import Control.Concurrent
import Optics
import Control.Monad
import qualified Data.Text as T
import qualified Di
import qualified DiPolysemy as DiP
import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.State as P
import System.Environment (getEnv)
import TextShow
import Service.Test (testService)
import Service.List (listService)
import PostgresDB
import Database
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
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
import Calamity.Commands.Context (ctxChannelID)



data MyViewState = MyViewState
  { numOptions :: Int
  , selected :: Maybe T.Text
  }

$(makeFieldLabelsNoPrefix ''MyViewState)

connStr :: ConnectionString
connStr = "host=localhost dbname=postgres user=postgres password=postgres port=5432"

addTodoID :: [Int64] -> [(String, (String, (UTCTime, Int)))] -> [(Int64, (String, (String, (UTCTime, Int))))]
addTodoID [] [] = []
addTodoID (a:ax) (b:bx) = [(a,b)] ++ addTodoID ax bx
  
formatTodo :: [(Int64, (String, (String, (UTCTime, Int))))] -> [String]
formatTodo [] = []
formatTodo ((a, (b, (c, (d, e)))):ax)
    | e == 0 = [ (show a) ++ " - [] - " ++ b ++ " - " ++ c ++ " - " ++ (iso8601 d)] ++ formatTodo ax
    | otherwise = [ (show a) ++ " - [X] - " ++ b ++ " - " ++ c ++ " - " ++ (iso8601 d)] ++ formatTodo ax

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

returnFunc :: [(Int64, (String, (String, (UTCTime, Int))))] -> String
returnFunc a
  | (intercalate "\n" $ formatTodo a) /= "" = (intercalate "\n" $ formatTodo a)
  | otherwise = "no todo data"

main :: IO ()
main = do
  Di.new $ \di ->
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . runPersistWith connStr
      . useConstantPrefix "!"
      . useFullContext
      . runBotIO (BotToken "MTAzNzAxNzYwOTAzNzY3NjU4NQ.GPB_mz.lPXBh0evvPexT8HuCgFmidf85iThgaMpXLeXkI") defaultIntents $ do 
        db $ runMigration migrateAll
        void $ addCommands $ do
          command @'[] "all" \ctx -> do
            allTodoRaw <- db $ selectList [TodoServer_id ==. (show (ctxChannelID ctx))] []
            let allTodo = (todoTitle &&& todoDescription &&& todoDeadline_date &&& todoStatus ) . entityVal <$> (allTodoRaw :: [Entity Todo])
            let allTodoID = fromSqlKey . entityKey <$> (allTodoRaw :: [Entity Todo])
            let allTodoWithID = addTodoID allTodoID allTodo
            void $ tell @T.Text ctx $ T.pack (returnFunc allTodoWithID) 

          command @'[] "bye" \ctx -> do
            void $ tell @T.Text ctx "bye!"
            stopBot