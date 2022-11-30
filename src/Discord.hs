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

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (useFullContext, FullContext)
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
import Data.Time.Clock(UTCTime)
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
import Calamity.Commands.Context (ctxChannelID)

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

getMessageContentParams :: T.Text -> [T.Text]
getMessageContentParams t = tail $ map T.strip (T.splitOn " " t)

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

          command @'[] "editdesc" \ctx -> do
            let updateList = getMessageContentParams $ ctx ^. #message % #content
            let todoid = head updateList
            let newDescription = T.intercalate " " $ tail updateList

            -- todoRaw <- db $ getBy $ UniqueTitle todotitle
            -- case todoRaw of
            --   Nothing -> pure Nothing
            --   Just (Entity todoid (Todo {}))  -> do
            --     db_ $ update todoid [TodoDescription =. newDescription]
            --     pure $ Just todoid

            db_ $ update (toSqlKey . read . T.unpack $ todoid) [TodoDescription =. T.unpack newDescription]
            void $ tell @T.Text ctx "description updated"

          command @'[] "bye" \ctx -> do
            void $ tell @T.Text ctx "bye!"
            stopBot