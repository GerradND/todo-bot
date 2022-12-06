{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Query (checkUncheckQuery, deleteTodoQuery) where

import           Calamity (tell, BotC)
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           Calamity.Metrics.Noop
import           Control.Concurrent
import           Control.Monad
import           Data.Char (isDigit)
import qualified Data.Text                   as T
import           Database
import           Database.Persist
import           Database.Persist.Postgresql as Psql
import           Database.Persist.TH
import           Database.Persist.Types
import qualified Di
import           Optics
import qualified Polysemy                    as P
import           PostgresDB

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

printResponse ::   
    ( BotC r
    , P.Members
    '[ Persistable
    ] r
    ) => FullContext -> T.Text -> P.Sem r ()
printResponse ctx txt = void $ tell @T.Text ctx $ txt