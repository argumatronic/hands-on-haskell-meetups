{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Task(..)
    , TaskId(..)
    , TaskFields(..)
    , TaskTitle(..)
    , listTasksIO
    , newTaskIO
    , deleteTaskIO
    , updateTaskIO
    ) where

import Control.Monad (void)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.Result
import Data.Maybe (listToMaybe)
import Data.String (IsString)
import Data.Text (Text, pack)

data Task = Task TaskId TaskFields deriving Show
data TaskFields = TaskFields TaskTitle deriving Show

newtype TaskId = TaskId { unTaskId :: Int } deriving (Eq, Show, Param, Result)
newtype TaskTitle = TaskTitle { unTaskTitle :: String } deriving (Eq, Show, Param, Result)
-- add completion, start/finish date, possibility of repeating task, notes field? update could update
-- any of those then? rather than just flipping a boolean flag. priority field.
-- task dependencies.

-- List
listTasksIO :: Connection -> IO [Task]
listTasksIO conn = do
  res <- listTasksDb conn
  return $ fmap tupleToTask res

tupleToTask :: (TaskId, TaskTitle) -> Task
tupleToTask (taskId, taskTitle) = Task taskId (TaskFields taskTitle)

listTasksDb :: Connection -> IO [(TaskId, TaskTitle)]
listTasksDb conn = query_ conn "SELECT id, title FROM todo;"

-- New
newTaskIO :: Connection -> TaskFields -> IO Int
newTaskIO conn (TaskFields taskTitle) = do
  void $ execute conn "INSERT INTO todo (title) VALUES (?)" (Only taskTitle)
  rawTaskId <- insertID conn
  return $ fromIntegral rawTaskId

-- Delete
deleteTaskIO :: Connection -> TaskId -> IO ()
deleteTaskIO conn taskId = void $ execute conn "DELETE FROM todo WHERE id=?" (Only taskId)

-- Update
updateTaskIO :: Connection -> TaskId -> TaskTitle -> IO ()
updateTaskIO conn taskId newTaskTitle =
  void $ execute conn "UPDATE todo SET title=? WHERE id=?" (newTaskTitle, taskId)
