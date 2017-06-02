module Main where

import Options.Applicative
import Data.Char (toUpper)
import Data.Monoid ((<>))

data Welcome = Welcome { name :: String
                       , excited :: Bool  }
-- the first argument is a string; the switch is a Bool

runWithOptions :: Welcome -> IO ()
runWithOptions opts =
  putStrLn $ transform $
    "Welcome to meetup, " ++ name opts ++ "!"
  where
    transform = if excited opts then map toUpper else id
-- switch to all caps if the "excited" option is True

main :: IO ()
main = execParser opts >>= runWithOptions
-- execParser :: ParserInfo a -> IO a
  where
    parser = Welcome <$> argument str (metavar "NAME")  -- the metavar sets usage information
                     <*> switch (short 'e' <>
                               long "excited")
    -- opts = info parser mempty

    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )


-- info :: Parser a -> InfoMod a -> ParserInfo a
-- opts gives us a ParserInfo Welcome
-- >>= :: Monad m => m a -> (a -> m b) -> m b
-- execParser opts >>= runWithOptions
-- takes opts (:: ParserInfo Welcome) and returns an IO Welcome value as the `m a` (IO is the m)
-- runWithOptions needs the Welcome value from IO Welcome as its argument, returns an IO (), so it's (a -> m b) (IO is the m)


-- to run (this isn't the name of the executable in this package tho)
-- $ stack exec -- optex "julie"
-- $ stack exec -- optex "julie" -e

-- or in ghci:
-- λ> :main "julie"
--
-- λ> :main "julie" -e
