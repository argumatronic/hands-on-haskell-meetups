module CommandLineParser where

import Data.Monoid ((<>))
import Options.Applicative

import Lib

data Command = New TaskTitle
             | List
             | Update TaskId TaskTitle
             | Delete TaskId
             deriving (Eq, Show)

parserNew :: Parser Command
parserNew = New <$> (TaskTitle <$> strArgument (metavar "TASK_TITLE"))

parserList :: Parser Command
parserList = pure List

parserUpdate :: Parser Command
parserUpdate =  Update <$> (TaskId <$> argument auto (metavar "TASK_ID")) <*> (TaskTitle <$> strArgument (metavar "TASK_TITLE"))

parserDelete :: Parser Command
parserDelete = Delete <$> (TaskId <$> argument auto (metavar "TASK_ID"))

parserCommand :: Parser Command
parserCommand = subparser $
    command "new" (parserNew `withInfo` "Add new entry.") <>
    command "list" (parserList `withInfo` "List tasks.") <>
    command "update" (parserUpdate `withInfo` "Update a task.") <>
    command "delete" (parserDelete `withInfo` "Delete a task.")

parserInfoCommand :: ParserInfo Command
parserInfoCommand = info (parserCommand) (progDesc "Manage todo list.")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
