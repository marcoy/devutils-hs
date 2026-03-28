module DevUtils.Cmd.Xml
  ( xmlHandler
  ) where


import           Conduit                          (runConduit, stdoutC, (.|))
import           Control.Monad.Catch              (MonadThrow)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Data.ByteString                  (ByteString)
import           Data.Conduit                     (ConduitT, await, yield)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           DevUtils.Cmd.Internal.Conduit.IO (fpInputSource)
import           DevUtils.Opts                    (XmlOptions (..))


xmlHandler :: forall m. (MonadResource m, MonadThrow m) => XmlOptions -> m ()
xmlHandler opts = do
  let source = fpInputSource opts.inputFp
      n      = max 1 opts.unescaped
  runConduit $ source
            .| xmlUnescapeC n
            .| stdoutC


xmlUnescapeC :: (Monad m) => Int -> ConduitT ByteString ByteString m ()
xmlUnescapeC n = do
  bsMaybe <- await
  case bsMaybe of
    Nothing -> pure ()
    Just bs -> do
      let txt = applyN n unescapeXml (decodeUtf8 bs)
      yield (encodeUtf8 txt)
      xmlUnescapeC n
  where
    applyN n f = foldr (.) id (replicate n f)


unescapeXml :: Text -> Text
unescapeXml = T.replace "&amp;" "&"
            . T.replace "&lt;" "<"
            . T.replace "&gt;" ">"
            . T.replace "&quot;" "\""
            . T.replace "&apos;" "'"
