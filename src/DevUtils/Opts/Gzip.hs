{-# LANGUAGE StrictData #-}
module DevUtils.Opts.Gzip
  ( GzipOptions (..)

  , gzipOpts
  ) where


import           GHC.Generics        (Generic)
import           Options.Applicative
import           TextShow            (TextShow)
import           TextShow.Generic    (FromGeneric (..))


data GzipOptions = GzipOptions
  { decompressMode :: Bool
  , base64         :: Bool
  , inputFp        :: Maybe FilePath
  } deriving stock (Show, Eq, Generic)
    deriving TextShow via FromGeneric GzipOptions


gzipOpts :: Parser GzipOptions
gzipOpts = GzipOptions
  <$> switch (short 'd' <> help "Decompress instead of compressing")
  <*> switch (short 'b' <> long "b64" <> help "Base64 encode/decode")
  <*> optional (strArgument (metavar "<FILE>" <> help "Optional. Default to stdin. '-' means stdin also"))
