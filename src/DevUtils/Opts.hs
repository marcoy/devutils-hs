module DevUtils.Opts
  ( DevUOptions (..)
  , DevUCommands (..)
  , execArgsParser

  , module Os
  ) where


import           DevUtils.Opts.ULID  as Os
import           DevUtils.Opts.UUID  as Os
import           GHC.Generics        (Generic)
import           Options.Applicative
import           TextShow            (TextShow)
import           TextShow.Generic    (FromGeneric (..))


newtype DevUOptions = DevUOptions { cmd :: DevUCommands }
                      deriving stock (Show, Eq, Generic)
                      deriving TextShow via FromGeneric DevUOptions

data DevUCommands = UUID UUIDOptions
                  | ULID ULIDOptions
                  deriving stock (Show, Eq, Generic)
                  deriving TextShow via FromGeneric DevUCommands


devUOpts :: Parser DevUOptions
devUOpts = DevUOptions <$>
  subparser
    ( command "uuid"
        (info (UUID <$> uuidOpts <**> helper) (progDesc "Generate UUID"))
   <> command "ulid"
        (info (ULID <$> ulidOpts <**> helper) (progDesc "Generate ULID"))
   <> metavar "<SUB_COMMAND>"
    )

execArgsParser :: IO DevUOptions
execArgsParser = customExecParser prefs (info (devUOpts <**> helper) (progDesc "Dev Utils"))
  where
    prefs = defaultPrefs { prefShowHelpOnEmpty = True
                         , prefShowHelpOnError = True
                         }
