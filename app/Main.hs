module Main where

import PostgresDB
import Discord

main :: IO ()
main = do
    -- PostgresDB.main
    Discord.main
    -- Database.getUserById 1