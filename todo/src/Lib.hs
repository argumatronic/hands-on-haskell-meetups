{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( Task(..)
    , TaskId(..)
    , TaskFields(..)
    , TaskTitle(..)
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

data Task = Task TaskId TaskFields deriving Show
data TaskFields = TaskFields TaskTitle deriving Show

newtype TaskId = TaskId { unTaskId :: Int } deriving (Eq, Show)
newtype TaskTitle = TaskTitle { unTaskTitle :: String } deriving (Eq, Show)

-- newtype TaskCompleted = TaskCompleted { unTaskCompleted :: Bool } deriving (Eq, Show)
-- add start/finish date, possibility of repeating task, notes field? update could update
-- any of those then? rather than just flipping a boolean flag. priority field.
-- task dependencies.
