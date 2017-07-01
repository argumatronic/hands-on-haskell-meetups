{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Database.MySQL.Simple
import Options.Applicative

import CommandLineParser
import Lib

main :: IO ()
main = do
    -- Replace the MySQL connection information below:
    let connectInfo = defaultConnectInfo
          { connectHost = "localhost"
          , connectUser = "root"
          , connectPassword = "root"
          , connectDatabase = "todo"
          }
    conn <- connect connectInfo
    command <- customExecParser (prefs showHelpOnEmpty) parserInfoCommand
    case command of
      New taskTitle -> newTask conn taskTitle
      List -> listTasks conn
      Update taskId taskTitle -> updateTask conn taskId taskTitle
      Delete taskId -> deleteTask conn taskId

newTask :: Connection -> TaskTitle -> IO ()
newTask conn taskTitle = do
  rawTaskId <- newTaskIO conn $ TaskFields taskTitle
  putStrLn $ "Created a new task \"" ++ (unTaskTitle taskTitle) ++ "\" with ID " ++ (show rawTaskId)

listTasks :: Connection -> IO ()
listTasks conn = do
  tasks <- listTasksIO conn
  forM_ tasks printTask

printTask :: Task -> IO ()
printTask (Task taskId (TaskFields taskTitle)) =
  let rawTaskId = unTaskId taskId
      rawTaskTitle = unTaskTitle taskTitle
  in putStrLn $ (show rawTaskId) ++ ": " ++ rawTaskTitle

updateTask :: Connection -> TaskId -> TaskTitle -> IO ()
updateTask conn taskId newTaskTitle = do
  updateTaskIO conn taskId newTaskTitle
  putStrLn $ "Updated task " ++ (show $ unTaskId $ taskId) ++ " with new title: " ++ (unTaskTitle newTaskTitle)

deleteTask :: Connection -> TaskId -> IO ()
deleteTask conn taskId = do
  deleteTaskIO conn taskId
  putStrLn $ "Deleted task " ++ (show $ unTaskId $ taskId)

-- $ stack exec -- todo new "write"
-- $ stack exec -- todo delete 1
-- $ stack exec -- todo new --help

-- in ghci you can do this to play with it:
-- λ> :main new
-- Missing: TASK_NAME
--
-- Usage: <interactive> new TASK_NAME
--   Add new entry.
-- *** Exception: ExitFailure 1
--
-- λ> :main new "make todo list"
-- New "make todo list"
-- it :: ()
--
-- λ> :main new "write"
-- New "write"
-- it :: ()
