module Lib
    ( someFunc
    ) where

import Control.Monad (void)
-- import Database.PostgreSQL.Simple
--         ( Connection
--         , ConnectInfo(..)
--         , Only(..)
--         , connect
--         , execute
--         , fromOnly
--         , query
--         , query_
--         )
-- import Database.PostgreSQL.Simple.FromField (FromField)
-- import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Maybe (listToMaybe)
import Data.String (IsString)
import Data.Text (Text, pack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Task = Task TaskId TaskFields deriving Show
data TaskFields = TaskFields TaskLabel TaskCompleted deriving Show

newtype TaskId = TaskId { unTaskId :: Int } deriving (Eq, Show)
newtype TaskLabel = TaskLabel { unTaskLabel :: Text } deriving (Eq, Show)
newtype TaskCompleted = TaskCompleted { unTaskCompleted :: Bool } deriving (Eq, Show)
-- add start/finish date, possibility of repeating task, notes field? update could update 
-- any of those then? rather than just flipping a boolean flag. priority field. 
-- task dependencies. 
