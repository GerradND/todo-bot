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
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import PostgresDB
import Database

data MyViewState = MyViewState
  { numOptions :: Int
  , selected :: Maybe T.Text
  }

connStr :: ConnectionString
connStr = "host=localhost dbname=postgres user=postgres password=root port=5432"

$(makeFieldLabelsNoPrefix ''MyViewState)

main :: IO ()
main = Di.new $ \di -> void . P.runFinal . P.embedToFinal @IO . DiP.runDiToIO di
        . runCacheInMemory
        . runMetricsNoop
        . runPersistWith Discord.connStr
        . useConstantPrefix "!"
        . useFullContext
        . runBotIO (BotToken "MTAzNzAxNzYwOTAzNzY3NjU4NQ.GPB_mz.lPXBh0evvPexT8HuCgFmidf85iThgaMpXLeXkI") defaultIntents $ do
          addCommands $ do
            helpCommand
            -- just some examples

            command @'[] "utest" \ctx -> do
              void $ tell @T.Text ctx $ "got user: " <> ctx ^. #user % #username <> ", with message: " <> ctx ^. #message % #content
            command @'[] "test" \ctx -> do
              void $ tell @T.Text ctx "testing..."
            command @'[] "bye" \ctx -> do
              void $ tell @T.Text ctx "bye!"
              -- runPersistWithConnection Discord.connStr $ insert _
              stopBot

            -- views!

            command @'[] "components" \ctx -> do
              let view options = do
                    ~(add, done) <- I.row do
                      add <- I.button ButtonPrimary "add"
                      done <- I.button ButtonPrimary "done"
                      pure (add, done)
                    s <- I.select options
                    pure (add, done, s)
              let initialState = MyViewState 1 Nothing
              s <- P.evalState initialState $
                I.runView (view ["0"]) (tell ctx) \(add, done, s) -> do
                  when add do
                    n <- P.gets (^. #numOptions)
                    let n' = n + 1
                    P.modify' (#numOptions .~ n')
                    let options = map (T.pack . show) [0 .. n]
                    I.replaceView (view options) (void . I.edit)

                  when done do
                    finalSelected <- P.gets (^. #selected)
                    I.endView finalSelected
                    I.deleteInitialMsg
                    void . I.respond $ case finalSelected of
                      Just x -> "Thanks: " <> x
                      Nothing -> "Oopsie"

                  case s of
                    Just s' -> do
                      P.modify' (#selected ?~ s')
                      void I.deferComponent
                    Nothing -> pure ()
              P.embed $ print s

            -- more views!

            command @'[] "cresponses" \ctx -> do
              let view = I.row do
                    a <- I.button ButtonPrimary "defer"
                    b <- I.button ButtonPrimary "deferEph"
                    c <- I.button ButtonPrimary "deferComp"
                    d <- I.button ButtonPrimary "modal"
                    pure (a, b, c, d)

                  modalView = do
                    a <- I.textInput TextInputShort "a"
                    b <- I.textInput TextInputParagraph "b"
                    pure (a, b)

              I.runView view (tell ctx) $ \(a, b, c, d) -> do
                when a do
                  void I.defer
                  P.embed $ threadDelay 1000000
                  void $ I.followUp @T.Text "lol"

                when b do
                  void I.deferEphemeral
                  P.embed $ threadDelay 1000000
                  void $ I.followUpEphemeral @T.Text "lol"

                when c do
                  void I.deferComponent
                  P.embed $ threadDelay 1000000
                  void $ I.followUp @T.Text "lol"

                when d do
                  void . P.async $ do
                    I.runView modalView (void . I.pushModal "lol") $ \(a, b) -> do
                      P.embed $ print (a, b)
                      void $ I.respond ("Thanks: " <> a <> " " <> b)
                      I.endView ()

                pure ()

          react @( 'CustomEvt (CommandInvoked FullContext)) $ \(CommandInvoked ctx) -> do
            DiP.info $ "Command invoked by " <> ctx ^. #user % #username <> ":\n" <> ctx ^. #message % #content