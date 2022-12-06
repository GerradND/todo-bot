{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Query (checkUncheckQuery, getAllTODOQuery) where

import           Calamity (tell, BotC)
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext, ctxChannelID)
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
import           Data.Time.Clock(UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Control.Arrow ((&&&))

checkUncheckQuery :: 
    ( BotC r
    , P.Members
    '[ Persistable
    ] r
    ) => FullContext -> T.Text -> Int -> P.Sem r ()
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