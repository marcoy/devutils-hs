{-# LANGUAGE QuasiQuotes #-}
module DevUtils.Data.Epoch.EpochUnit
  ( EpochUnit (..)

  , epochUnitReadM
  ) where


import           Data.String.Interpolate (i)
import           GHC.Generics            (Generic)
import           Options.Applicative     (ReadM, eitherReader)
import           TextShow                (TextShow)
import           TextShow.Generic        (FromGeneric (..))


data EpochUnit = EpochSeconds
               | EpochMillis
               deriving stock (Show, Eq, Generic)
               deriving TextShow via FromGeneric EpochUnit


epochUnitReadM :: ReadM EpochUnit
epochUnitReadM = eitherReader $ \case
  "seconds" -> Right EpochSeconds
  "millis"  -> Right EpochMillis
  other     -> Left [i|Unknown epoch unit #{other}. Use either "seconds" or "millis"|]
