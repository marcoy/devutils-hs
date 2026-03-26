{-# LANGUAGE StrictData #-}

module DevUtils.Opts.ULID
  ( ULIDOptions (..)

  , ulidOpts
  ) where


import           Data.Word           (Word64)
import           GHC.Generics        (Generic)
import           Options.Applicative
import           TextShow            (TextShow)
import           TextShow.Generic    (FromGeneric (..))


data ULIDOptions = ULIDOptions
                   { seed      :: Maybe Word64
                   , upperCase :: Bool
                   } deriving stock (Show, Eq, Generic)
                     deriving TextShow via FromGeneric ULIDOptions


ulidOpts :: Parser ULIDOptions
ulidOpts = ULIDOptions
  <$> optional (option auto (long "seed" <> metavar "<SEED>" <> help "Seed for the PRNG"))
  <*> switch (short 'U' <> help "Output the ULID in uppercase")
