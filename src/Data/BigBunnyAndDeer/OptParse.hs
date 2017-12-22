module Data.BigBunnyAndDeer.OptParse where

import Data.Default
import Options.Applicative
import Data.Semigroup
import Data.BigBunnyAndDeer.Type

configP :: Parser BigBunnyConfig
configP = BigBunnyConfig
  <$> strOption
      (  long "db_url"
      <> metavar "TEXT_DB_URL"
      <> value (textDatabaseURL def))
  <*> strOption
      (  long "auth_file"
      <> metavar "AUTH_FILE"
      <> value (authFilePath def))
  <*> strOption
      (  long "deer_file"
      <> metavar "DEER_INFO_FILE"
      <> value (deerInfoFilePath def))
