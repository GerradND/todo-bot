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

import           Calamity
import           Calamity.Cache.InMemory
import           Control.Concurrent
import           Calamity.Commands
import           Calamity.Commands.Context (useFullContext, ctxChannelID, FullContext)
import qualified Calamity.Interactions        as I
import           Calamity.Metrics.Noop
import           Control.Arrow ((&&&))
import           Control.Monad
import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.Char (isDigit)
import           Data.List
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Time(defaultTimeLocale, parseTimeM)
import           Data.Time.Clock(UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)

import           Database
import           Database.Persist
import           Database.Persist.Postgresql  as Psql
import           Database.Persist.TH
import           Database.Persist.Types
import           Debug.Trace
import qualified Di
import qualified DiPolysemy                   as DiP
import           GHC.Int (Int64)
import           Optics
import qualified Polysemy                     as P
import qualified Polysemy.Async               as P
import qualified Polysemy.State               as P
import qualified Polysemy.AtomicState         as P
import qualified Polysemy.Fail                as P
import qualified Polysemy.Reader              as P
import           PostgresDB
import           Query
import           TextShow

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

getMessageContentParamsWithDelimiter :: String -> T.Text -> [T.Text]
getMessageContentParamsWithDelimiter d t = tail $ map T.strip (T.splitOn (T.pack d) t)

deadlinesToUTCTimeString :: T.Text -> T.Text -> String
deadlinesToUTCTimeString deadlineDate deadlineTime = T.unpack (deadlineDate <> " " <> deadlineTime)

printHelpException :: (BotC r) => FullContext -> T.Text -> P.Sem r ()
printHelpException ctx cmd = void $ tell @T.Text ctx $ "Wrong format! For help: !help " <> cmd

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
            db_ $ Psql.insert $ UserD (ctx ^. #user % #username) (ctx ^. #message % #content)

          command @'[] "user" \ctx -> do
            let user = ctx ^. #user % #username
            userId <- db $ selectList [UserDName ==. user] [LimitTo 1]
            void $ tell @T.Text ctx $ "user: " <> (userDName . entityVal . head) userId

          void $ help (const "Mark completed todo list.\nExample: !check 1")
            $ command @'[T.Text] "check" \ctx todoId -> do
            checkUncheckQuery ctx todoId 1

          void $ help (const "Unmark uncomplete todo list.\nExample: !uncheck 1")
            $ command @'[T.Text] "uncheck" \ctx todoId-> do
            checkUncheckQuery ctx todoId 0

          command @'[] "all" \ctx -> do
            allTodoRaw <- db $ selectList [TodoServer_id ==. (show (ctxChannelID ctx))] []
            let allTodo = (todoTitle &&& todoDescription &&& todoDeadline_date &&& todoStatus ) . entityVal <$> (allTodoRaw :: [Entity Todo])
            let allTodoID = fromSqlKey . entityKey <$> (allTodoRaw :: [Entity Todo])
            let allTodoWithID = addTodoID allTodoID allTodo
            void $ tell @T.Text ctx $ T.pack (returnFunc allTodoWithID)

          void $ help (const "Edit todo title by id.\nFormat: **!edit-title <id> <title>**\nExample: **!edit-title 1 Create Progress Report**")
            $ command @'[] "edit-title" \ctx -> do
              let updateList = getMessageContentParamsWithDelimiter " " $ ctx ^. #message % #content
              let updateId = head updateList
              let newTitle = T.intercalate " " $ tail updateList

              if T.all isDigit updateId && length updateList >= 2
                then do editTitleQuery ctx updateId newTitle
              else
                printHelpException ctx "edit-title"

          void $ help (const "Edit todo description by id.\nFormat: **!edit-desc <id> <description>**\nExample: **!edit-desc 1 limit 5 pages**")
            $ command @'[] "edit-desc" \ctx -> do
              let updateList = getMessageContentParamsWithDelimiter " " $ ctx ^. #message % #content
              let updateId = head updateList
              let newDescription = T.intercalate " " $ tail updateList

              if T.all isDigit updateId && length updateList >= 2
                then do editDescQuery ctx updateId newDescription
              else
                printHelpException ctx "edit-desc"

          void $ help (const "Edit todo deadline date by id.\nFormat: **!edit-date <id> <YYYY:MM:DD> <HH:mm>**\nExample: **!edit-date 1 2022-12-31 21:00**")
            $ command @'[] "edit-date" \ctx -> do
              let updateList = getMessageContentParamsWithDelimiter " " $ ctx ^. #message % #content
              case updateList of 
                [updateId, newDate, newTime] -> do
                  if T.all isDigit updateId
                    then do 
                      let deadlineDateString = deadlinesToUTCTimeString newDate newTime
                      case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" deadlineDateString of
                        Nothing -> printHelpException ctx "edit-date"
                        Just deadlineDate -> editDateQuery ctx updateId deadlineDate
                  else
                    printHelpException ctx "edit-date"
                _ -> printHelpException ctx "edit-date"
              
          void $ help (const "Edit todo by id.\nFormat: **!edit <id> | <title> | <description> | <YYYY:MM:DD> <HH:mm>**\nExample: **!edit 1 | Create Report | limit 5 pages | 2022-12-31 21:00**")
            $ command @'[] "edit" \ctx -> do
              let updateList = getMessageContentParamsWithDelimiter " " $ ctx ^. #message % #content
              let updateId = head updateList

              let paramList = getMessageContentParamsWithDelimiter "|" $ ctx ^. #message % #content
              case paramList of 
                [newTitle, newDescription, newDateTime] -> do
                  if T.all isDigit updateId
                    then do
                      let dateTimeList = T.splitOn " " newDateTime
                      case dateTimeList of 
                        [newDate, newTime] -> do
                          let deadlineDateString = deadlinesToUTCTimeString newDate newTime
                          case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" deadlineDateString of
                            Nothing -> printHelpException ctx "edit"
                            Just deadlineDate -> editTodoQuery ctx updateId newTitle newDescription deadlineDate
                        _ -> printHelpException ctx "edit"
                  else printHelpException ctx "edit"
                _ -> printHelpException ctx "edit"

          command @'[] "bye" \ctx -> do
            void $ tell @T.Text ctx "bye!"
            stopBot