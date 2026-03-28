{-# LANGUAGE StrictData #-}
module DevUtils.Opts.Epoch
  ( EpochOptions (..)

  , epochOpts
  ) where


import           Data.Int                      (Int64)
import           Data.Text                     (Text)
import           DevUtils.Data.Epoch.EpochUnit (EpochUnit, epochUnitReadM)
import           GHC.Generics                  (Generic)
import           Options.Applicative
import           TextShow                      (TextShow)
import           TextShow.Generic              (FromGeneric (..))


data EpochOptions = EpochOptions
  { epochUnit      :: Maybe EpochUnit
  , epochTimestamp :: Maybe Int64
  } deriving stock (Show, Eq, Generic)
    deriving TextShow via FromGeneric EpochOptions

epochOpts :: Parser EpochOptions
epochOpts = EpochOptions
  <$> optional (option epochUnitReadM (long "unit" <> short 'u' <> metavar "<EPOCH_UNIT>" <> help "Epoch timestamp unit (seconds or millis)"))
  <*> optional (argument auto (metavar "<EPOCH_TIMESTAMP>" <> help "A timestamp"))
