{-# LANGUAGE StrictData #-}
module DevUtils.Opts.Base64
  ( Base64Options (..)

  , base64Opts
  ) where


import           GHC.Generics        (Generic)
import           Options.Applicative
import           TextShow            (TextShow)
import           TextShow.Generic    (FromGeneric (..))


data Base64Options = Base64Options
  { decodeMode :: Bool
  , inputFp    :: Maybe FilePath
  } deriving stock (Show, Eq, Generic)
    deriving TextShow via FromGeneric Base64Options


base64Opts :: Parser Base64Options
base64Opts = Base64Options
  <$> switch (short 'd' <> help "Decode Base64 instead of encoding")
  <*> optional (strArgument (metavar "<FILE>" <> help "Optional. Default to stdin. '-' means stdin also"))
