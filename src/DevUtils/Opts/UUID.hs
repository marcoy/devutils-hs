{-# LANGUAGE StrictData #-}

module DevUtils.Opts.UUID
  ( UUIDOptions (..)

  , uuidOpts
  ) where


import           GHC.Generics        (Generic)
import           Options.Applicative
import           TextShow            (TextShow)
import           TextShow.Generic    (FromGeneric (..))


newtype UUIDOptions = UUIDOptions { upperCase :: Bool }
                      deriving stock (Show, Eq, Generic)
                      deriving TextShow via FromGeneric UUIDOptions

uuidOpts :: Parser UUIDOptions
uuidOpts = UUIDOptions <$> switch (short 'U' <> help "Output the generated UUID in upper case")
