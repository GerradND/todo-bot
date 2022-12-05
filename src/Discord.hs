{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
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

import Calamity.Commands.Context as Ctx(FullContext(user), useFullContext, ctxChannelID, ctxMessage)

import Data.Maybe
import System.IO.Unsafe
import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
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
import qualified Polysemy.AtomicState      as P
import qualified Polysemy.Fail             as P
import qualified Polysemy.Reader           as P
import System.Environment (getEnv)
import TextShow
import Database.Persist
import Database.Persist.Postgresql as Psql
import Database.Persist.TH
import Database.Persist.Types
import Data.Time.Clock(UTCTime, getCurrentTime)
import Data.Time(defaultTimeLocale, parseTimeM)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Data.Typeable
import PostgresDB
import Database
import Data.List
import Debug.Trace
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Int (Int64)
import qualified Data.Text as T
import Calamity.Types.Model.Channel.Message as M
import Calamity.Types.Model.User as U


data MyViewState = MyViewState
  { numOptions :: Int
  , selected :: Maybe T.Text
  }

$(makeFieldLabelsNoPrefix ''MyViewState)

connStr :: Psql.ConnectionString
connStr = "host=localhost dbname=todobot user=postgres password=root port=5432"

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

getMessageContentParams :: String -> T.Text -> [T.Text]
getMessageContentParams d t = tail $ map T.strip (T.splitOn (T.pack d) t)

getNthElement :: Int -> [a] -> a
getNthElement _ [] = error "Invalid command!"
getNthElement n (x:xs)
  | n <= 0 = x
  | otherwise = getNthElement (n-1) xs

deadlinesToUTCTimeString :: T.Text -> T.Text -> String
deadlinesToUTCTimeString deadlineDate deadlineTime = T.unpack (deadlineDate <> " " <> deadlineTime)

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
          helpCommand
          command @'[] "utest" \ctx -> do
            -- data Context = Context { ... , user :: User }
            -- #user :: Getter s a 
            -- data User = User { ... , username :: Text }
            -- Map s a
            void $ tell @T.Text ctx $ "got user: " <> ctx ^. #user % #username <> ", with message: " <> ctx ^. #message % #content
            DiP.info $ "Type: " <> show (typeOf $ ctx ^. #user % #username) <> ", value: " <> show (ctx ^. #user % #username)
            db_ $ Psql.insert $ UserD (ctx ^. #user % #username) (ctx ^. #message % #content)

          command @'[] "user" \ctx -> do
            let user = ctx ^. #user % #username
            userId <- db $ selectList [UserDName ==. user] [LimitTo 1]
            void $ tell @T.Text ctx $ "user: " <> (userDName . entityVal . head) userId

          command @'[] "all" \ctx -> do
            allTodoRaw <- db $ selectList [TodoServer_id ==. (show (ctxChannelID ctx))] []
            let allTodo = (todoTitle &&& todoDescription &&& todoDeadline_date &&& todoStatus ) . entityVal <$> (allTodoRaw :: [Entity Todo])
            let allTodoID = fromSqlKey . entityKey <$> (allTodoRaw :: [Entity Todo])
            let allTodoWithID = addTodoID allTodoID allTodo
            void $ tell @T.Text ctx $ T.pack (returnFunc allTodoWithID)

          void $ help (const "Edit todo title by id.\nFormat: !edit-title <id> <title>\nExample: !edit-title 1 Create Progress Report")
            $ command @'[] "edit-title" \ctx -> do
              let updateList = getMessageContentParams " " $ ctx ^. #message % #content
              let updateId = head updateList
              let newTitle = T.intercalate " " $ tail updateList

              let todoId = toSqlKey . read . T.unpack $ updateId
              todo <- db $ get todoId
              case todo of
                Nothing -> do
                  void $ tell @T.Text ctx "Todo not found!"
                Just (Todo {}) -> do
                  db_ $ update todoId [TodoTitle =. T.unpack newTitle]
                  void $ tell @T.Text ctx "Title updated!"

          void $ help (const "Edit todo description by id.\nFormat: !edit-desc <id> <description>\nExample: !edit-desc 1 Limit 5 pages")
            $ command @'[] "edit-desc" \ctx -> do
              let updateList = getMessageContentParams " " $ ctx ^. #message % #content
              let updateId = getNthElement 0 updateList
              let newDescription = T.intercalate " " $ tail updateList

              let todoId = toSqlKey . read . T.unpack $ updateId
              todo <- db $ get todoId
              case todo of
                Nothing -> do
                  void $ tell @T.Text ctx "Todo not found!"
                Just (Todo {}) -> do
                  db_ $ update todoId [TodoDescription =. T.unpack newDescription]
                  void $ tell @T.Text ctx "Description updated!"

          void $ help (const "Edit todo deadline date by id.\nFormat: !edit-date <id> <YYYY:MM:DD> <HH:mm>\nExample: !edit-date 1 2022-12-31 21:00")
            $ command @'[] "edit-date" \ctx -> do
              let updateList = getMessageContentParams " " $ ctx ^. #message % #content
              let updateId = getNthElement 0 updateList
              let newDate = getNthElement 1 updateList
              let newTime = getNthElement 2 updateList

              let deadlineDateString = deadlinesToUTCTimeString newDate newTime
              let deadlineDate = fromJust (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" deadlineDateString :: Maybe UTCTime)

              let todoId = toSqlKey . read . T.unpack $ updateId
              todo <- db $ get todoId
              case todo of
                Nothing -> do
                  void $ tell @T.Text ctx "Todo not found!"
                Just (Todo {}) -> do
                  db_ $ update todoId [TodoDeadline_date =. deadlineDate]
                  void $ tell @T.Text ctx "Deadline date updated!"

          void $ help (const "Edit todo by id.\nFormat: !edit <id> | <title> | <description> | <YYYY:MM:DD> <HH:mm>\nExample: !edit 1 | Create Report | Limit 5 pages | 2022-12-31 21:00")
            $ command @'[] "edit" \ctx -> do
              let updateList = getMessageContentParams " " $ ctx ^. #message % #content
              let updateId = getNthElement 0 updateList

              let paramList = getMessageContentParams "|" $ ctx ^. #message % #content
              let newTitle = getNthElement 0 paramList
              let newDescription = getNthElement 1 paramList
              let dateTimeList = T.splitOn " " $ getNthElement 2 paramList
              let newDate = getNthElement 0 dateTimeList
              let newTime = getNthElement 1 dateTimeList

              let deadlineDateString = deadlinesToUTCTimeString newDate newTime
              let deadlineDate = fromJust (parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" deadlineDateString :: Maybe UTCTime)

              let todoId = toSqlKey . read . T.unpack $ updateId
              todo <- db $ get todoId
              case todo of
                Nothing -> do
                  void $ tell @T.Text ctx "Todo not found!"
                Just (Todo {}) -> do
                  db_ $ update todoId [TodoTitle =. T.unpack newTitle, TodoDescription =. T.unpack newDescription, TodoDeadline_date =. deadlineDate]
                  void $ tell @T.Text ctx "Todo updated!"

          command @'[] "bye" \ctx -> do
            void $ tell @T.Text ctx "bye!"
            stopBot