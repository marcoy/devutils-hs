module DevUtils.Opts
  ( DevUOptions (..)
  , DevUCommands (..)
  , execArgsParser

  , module Os
  ) where


import           DevUtils.Opts.Base64 as Os
import           DevUtils.Opts.Epoch  as Os
import           DevUtils.Opts.Gzip   as Os
import           DevUtils.Opts.ULID   as Os
import           DevUtils.Opts.UUID   as Os

import           GHC.Generics         (Generic)
import           Options.Applicative
import           TextShow             (TextShow)
import           TextShow.Generic     (FromGeneric (..))


newtype DevUOptions = DevUOptions { cmd :: DevUCommands }
                      deriving stock (Show, Eq, Generic)
                      deriving TextShow via FromGeneric DevUOptions

data DevUCommands = UUID UUIDOptions
                  | ULID ULIDOptions
                  | Base64 Base64Options
                  | Gzip GzipOptions
                  | Epoch EpochOptions
                  deriving stock (Show, Eq, Generic)
                  deriving TextShow via FromGeneric DevUCommands


devUOpts :: Parser DevUOptions
devUOpts = DevUOptions <$>
  subparser
    ( command "uuid"
        (info (UUID <$> uuidOpts <**> helper) (progDesc "Generate UUID"))
   <> command "ulid"
        (info (ULID <$> ulidOpts <**> helper) (progDesc "Generate ULID"))
   <> command "b64"
        (info (Base64 <$> base64Opts <**> helper) (progDesc "Base64 encode/decode"))
   <> command "gzip"
        (info (Gzip <$> gzipOpts <**> helper) (progDesc "Gzip compress/decompress"))
   <> command "epoch"
        (info (Epoch <$> epochOpts <**> helper) (progDesc "Epoch timestamp related"))
   <> metavar "<SUB_COMMAND>"
    )

execArgsParser :: IO DevUOptions
execArgsParser = customExecParser prefs (info (devUOpts <**> helper) (progDesc "Dev Utils"))
  where
    prefs = defaultPrefs { prefShowHelpOnEmpty = True
                         , prefShowHelpOnError = True
                         }
