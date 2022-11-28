{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Database
  ( Persistable (..)
  , db
  , db_
  , runPersistWith
  ) where

import           Conduit
import           Control.Monad
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Text               (Text)
import           Database.Persist.Postgresql
import qualified Polysemy                as P

type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

data Persistable m a where
  Db :: DatabaseAction a -> Persistable m a

P.makeSem ''Persistable

-- | Like `db`, but discards the result.
db_ :: P.Member Persistable r => DatabaseAction a -> P.Sem r ()
db_ = void . db

-- userId <- (db $ (selectList [UserDName ==. user] [LimitTo 1]) :: DatabaseAction ([Entity UserD])) :: P.Sem r [Entity UserD]
-- userId :: [Entity UserD]

-- userId <- (db_ $ (selectList [UserDName ==. user] [LimitTo 1]) :: DatabaseAction ([Entity UserD])) :: P.Sem r ()
-- userId :: ()

-- | A `polysemy` effect handler for `persistent` actions.
runPersistWith :: P.Member (P.Embed IO) r => ConnectionString -> P.Sem (Persistable : r) a -> P.Sem r a
runPersistWith conn = P.interpret $ \case
  Db action ->
    P.embed
    . runResourceT
    . runStdoutLoggingT
    . withPostgresqlConn conn
    . runSqlConn
    $ action