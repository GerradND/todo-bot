{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Query (checkUncheckQuery, addQuery, editTitleQuery, editDescQuery, editDateQuery, editTodoQuery, deleteTodoQuery, getAllTODOQuery) where

import           Calamity (tell, BotC)
import           Calamity.Commands
import           Calamity.Commands.Context as Ctx (FullContext(user), ctxChannelID, ctxMessage)
import           Calamity.Types.Model.Channel.Message as M
import           Calamity.Types.Model.User as U
import           Calamity.Metrics.Noop
import           Control.Concurrent
import           Control.Monad
import           Data.Char (isDigit)
import qualified Data.Text                   as T
import qualified Data.List                  as L
import           Database
import           Database.Persist
import           Database.Persist.Postgresql as Psql
import           Database.Persist.TH
import           Database.Persist.Types
import qualified Di
import           Optics
import qualified Polysemy                    as P
import           PostgresDB
import           GHC.Int (Int64)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import           Control.Arrow ((&&&))
import qualified Data.Time as Data.Time.Clock.Internal.UTCTime

checkUncheckQuery :: 
    ( BotC r
    , P.Members
    '[ Persistable
    ] r
    ) => Ctx.FullContext -> T.Text -> Int -> P.Sem r ()
checkUncheckQuery ctx txt status = do 
    let message = T.unpack $ ctx ^. #message % #content
    let lengthMessage = length . words $ message
    if lengthMessage == 2 && (T.all isDigit txt)
        then do
            let todoId = toSqlKey . read . T.unpack $ txt
            todo <- db $ get todoId
            case todo of
                Nothing -> do
                    printResponse ctx "Todo not found!"
                Just (Todo {}) -> do
                    db_ $ update todoId [TodoStatus =. status]
                    if status == 1
                        then do
                            printResponse ctx "Todo checked!"
                    else
                        printResponse ctx "Todo unchecked!"
    else printResponse ctx "Wrong format!"

deadlinesToUTCTimeString :: T.Text -> T.Text -> String
deadlinesToUTCTimeString deadlineDate deadlineTime = T.unpack (deadlineDate <> " " <> deadlineTime)

-- MS: This is functions for add
getMessageContentParams :: String -> T.Text -> [T.Text]
getMessageContentParams d t = tail $ map T.strip (T.splitOn (T.pack d) t)

getParamsFromMessageContentParams :: String -> T.Text -> [T.Text]
getParamsFromMessageContentParams d t = map T.strip (T.splitOn (T.pack d) t)

addQuery :: 
    ( BotC r
    , P.Members
    '[ Persistable
    ] r
    ) => Ctx.FullContext -> P.Sem r ()
addQuery ctx = do
    let message = Ctx.ctxMessage ctx
    let addList = getMessageContentParams " " $ M.content message

    let paramsString = T.intercalate " " addList
    let paramList = getParamsFromMessageContentParams "|" paramsString
    case paramList of 
        [title, description, deadlineInDate, deadlineInTime] -> do
            let [title, description, deadlineInDate, deadlineInTime] = paramList
            let title = T.unpack $ head paramList
            let description = T.unpack $ (head . tail) paramList
            let deadlineInDate = (head . tail . tail) paramList
            let deadlineInTime = (head . tail . tail . tail) paramList
            let deadlineDateString = deadlinesToUTCTimeString deadlineInDate deadlineInTime

            case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" deadlineDateString of
                Nothing -> printResponse ctx "Invalid use of command! For help: !help add"
                Just deadlineDate -> do
                    createdDate <- P.embed getCurrentTime
                    let status = 0
                    let serverId = show (Ctx.ctxChannelID ctx)
                    let user = Ctx.user (ctx)
                    let username = U.username user
                    let discriminator = U.discriminator user
                    let createdBy = T.unpack $ username <> "#" <> discriminator

                    db_ $ Psql.insert (Todo title description deadlineDate createdDate status serverId createdBy)
                    printResponse ctx "Added new todo!"
        _ -> printResponse ctx "Invalid use of command! For help: !help add"
editTitleQuery :: 
    (BotC r, P.Members '[Persistable] r) 
    => FullContext -> T.Text -> T.Text -> P.Sem r ()
editTitleQuery ctx todoid newTitle = do
    let todoId = toSqlKey . read . T.unpack $ todoid
    todo <- db $ get todoId
    case todo of
        Nothing -> do
            printResponse ctx "Todo not found!"
        Just (Todo {}) -> do
            db_ $ update todoId [TodoTitle =. T.unpack newTitle]
            printResponse ctx "Title updated!"

editDescQuery :: 
    (BotC r, P.Members '[Persistable] r) 
    => FullContext -> T.Text -> T.Text -> P.Sem r ()
editDescQuery ctx todoid newDescription = do
    let todoId = toSqlKey . read . T.unpack $ todoid
    todo <- db $ get todoId
    case todo of
        Nothing -> do
            printResponse ctx "Todo not found!"
        Just (Todo {}) -> do
            db_ $ update todoId [TodoDescription =. T.unpack newDescription]
            printResponse ctx "Description updated!"

editDateQuery :: 
    (BotC r, P.Members '[Persistable] r) 
    => FullContext -> T.Text -> Data.Time.Clock.Internal.UTCTime.UTCTime -> P.Sem r ()
editDateQuery ctx todoid newDeadline_date = do
    let todoId = toSqlKey . read . T.unpack $ todoid
    todo <- db $ get todoId
    case todo of
        Nothing -> do
            printResponse ctx "Todo not found!"
        Just (Todo {}) -> do
            db_ $ update todoId [TodoDeadline_date =. newDeadline_date]
            printResponse ctx "Deadline date updated!"

editTodoQuery :: 
    (BotC r, P.Members '[Persistable] r) 
    => FullContext -> T.Text -> T.Text -> T.Text -> Data.Time.Clock.Internal.UTCTime.UTCTime -> P.Sem r ()
editTodoQuery ctx todoid newTitle newDescription newDeadline_date = do
    let todoId = toSqlKey . read . T.unpack $ todoid
    todo <- db $ get todoId
    case todo of
        Nothing -> do
            printResponse ctx "Todo not found!"
        Just (Todo {}) -> do
            db_ $ update todoId [TodoTitle =. T.unpack newTitle, TodoDescription =. T.unpack newDescription, TodoDeadline_date =. newDeadline_date]
            printResponse ctx "Todo updated!"
            
deleteTodoQuery :: 
    (BotC r, P.Members '[Persistable] r) 
    => FullContext -> T.Text -> P.Sem r ()
deleteTodoQuery ctx txt = do 
    let message = T.unpack $ ctx ^. #message % #content
    let lengthMessage = length . words $ message
    if lengthMessage == 2 && (T.all isDigit txt)
        then do
            let todoId = toSqlKey . read . T.unpack $ txt
            todo <- db $ get todoId
            case todo of
                Nothing -> do
                    printResponse ctx "Todo not found!"
                Just (Todo {}) -> do
                    db_ $ delete todoId
                    printResponse ctx "Todo has been successfully deleted!"
    else printResponse ctx "Wrong format! For help: !help delete-todo"

-- get all todo
returnFunc :: [Entity Todo] -> String
returnFunc a
  | (L.intercalate "\n" $ formatTodo a) /= "" = (L.intercalate "\n" $ formatTodo a)
  | otherwise = "no todo data"

formatTodo :: [Entity Todo] -> [String]
formatTodo [] = []
formatTodo ((Entity key (Todo title description deadline _createdDate status _serverId _createdBy)):ax)
  | status == 0 = [(show $ fromSqlKey key) <> " - [] - " <> title <> " - " <> description <> " - " <> iso8601 deadline] ++ formatTodo ax
  | otherwise = [(show $ fromSqlKey key) <> " - [X] - " <> title <> " - " <> description <> " - " <> iso8601 deadline] ++ formatTodo ax

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%Y-%-m-%-d %R"

getAllTODOQuery ::
    ( BotC r
    , P.Members
    '[ Persistable
    ] r
    ) => FullContext -> P.Sem r ()
getAllTODOQuery ctx = do
    allTodoRaw <- db $ selectList [TodoServer_id ==. (show (ctxChannelID ctx))] [Asc TodoDeadline_date, Asc TodoStatus] 
    printResponse ctx $ T.pack $ returnFunc allTodoRaw


printResponse ::   
    ( BotC r
    , P.Members
    '[ Persistable
    ] r
    ) => FullContext -> T.Text -> P.Sem r ()
printResponse ctx txt = void $ tell @T.Text ctx $ txt