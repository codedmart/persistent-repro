module Main where

import Prelude

import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Writer (censor, mapWriterT)
import Data.List (isSuffixOf)
import Data.String (fromString)
import Data.Text (Text, isInfixOf)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Quasi
import Database.Persist.TH
import GHC.Generics
import Options.Applicative
import System.Directory

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "schema/locationCapabilities.persistentmodels")

data Cmd = Cmd
  { connectionString :: String
  }

optsParser :: Parser Cmd
optsParser = Cmd <$> connStr
  where
  connStr =
    strArgument
      (  metavar "CONNECTION_STRING"
      <> help "Postgres connection string"
      )

main :: IO ()
main = do
    let
      noForeignKeys :: CautiousMigration -> CautiousMigration
      noForeignKeys = filter ((not . isReference) . snd)

      onlyForeignKeys :: CautiousMigration -> CautiousMigration
      onlyForeignKeys = filter (isReference . snd)

      isReference :: Text -> Bool
      isReference migration = "REFERENCES" `isInfixOf` migration

    Cmd c <- execParser parser

    runStdoutLoggingT
      $ withPostgresqlConn (fromString c)
      $ runReaderT
      $ runMigration
      $ mapWriterT (censor noForeignKeys) migrateAll

    runStdoutLoggingT
      $ withPostgresqlConn (fromString c)
      $ runReaderT
      $ runMigration
      $ mapWriterT (censor onlyForeignKeys) migrateAll
  where
  parser = info (optsParser <**> helper)
      (  fullDesc
      <> progDesc "Connection string for postgresql"
      <> header "persistent-repro"
      )

