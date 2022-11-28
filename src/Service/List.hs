module Service.List where

import qualified Data.Text as T
-- import PostgresDB (allTodoWithID)
import Database.Persist
import Database.Persist.Types
import Data.List


-- formatTodo :: [(Int64, (String, (String, (UTCTime, Int))))] -> [String]
-- formatTodo [] = []
-- formatTodo ((a, (b, (c, (d, e)))):ax)
--     | e == 0 = [ (show a) ++ " - [] - " ++ b ++ " - " ++ c ++ " - " ++ (iso8601 d)] ++ formatTodo ax
--     | otherwise = [ (show a) ++ " - [X] - " ++ b ++ " - " ++ c ++ " - " ++ (iso8601 d)] ++ formatTodo ax

-- iso8601 :: UTCTime -> String
-- iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

listService :: T.Text
listService = undefined