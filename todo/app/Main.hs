module Main where

import Options.Applicative
import Data.Monoid ((<>))

import Lib

data Command = New String
              | List String
              | Update String
              | Delete String
              deriving (Eq, Show)

parserNew :: Parser Command
parserNew = New <$> strArgument (metavar "TASK_NAME")

parserList :: Parser Command
parserList = List <$> strArgument (metavar "NUMBER_OF_TASKS")

parserUpdate :: Parser Command
parserUpdate = Update <$> strArgument (metavar "TASK_NAME")

parserDelete :: Parser Command
parserDelete = Delete <$> strArgument (metavar "TASK_NAME")


parserCommand :: Parser Command
parserCommand = subparser $
    command "new" (parserNew `withInfo` "Add new entry.") <>
    command "list" (parserList `withInfo` "List tasks.") <>
    command "update" (parserUpdate `withInfo` "Update a task without deleting.") <>
    command "delete" (parserDelete `withInfo` "Delete a task.")

parserInfoCommand :: ParserInfo Command
parserInfoCommand = info parserCommand (progDesc "Manage address book.")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


main :: IO ()
main = do
    command <- execParser parserInfoCommand
    print command

-- $ stack exec -- todo new "write"
-- New "write"
-- $ stack exec -- todo delete "write"
-- Delete "write"


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
