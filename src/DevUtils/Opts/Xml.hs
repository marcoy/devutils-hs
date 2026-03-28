{-# LANGUAGE StrictData #-}
module DevUtils.Opts.Xml
  ( XmlOptions (..)

  , xmlOpts
  ) where


import           GHC.Generics        (Generic)
import           Options.Applicative
import           TextShow            (TextShow)
import           TextShow.Generic    (FromGeneric (..))


data XmlOptions = XmlOptions
  { unescaped :: Int
  , inputFp   :: Maybe FilePath
  } deriving stock (Show, Eq, Generic)
    deriving TextShow via FromGeneric XmlOptions


xmlOpts :: Parser XmlOptions
xmlOpts = XmlOptions
  <$> countUs
  <*> optional (strArgument (metavar "<FILE>" <> help "Optional. Default to stdin"))

countUs :: Parser Int
countUs = length <$> many (flag' () (short 'u' <> help "Unescape XML. Can be specified multiple times"))
