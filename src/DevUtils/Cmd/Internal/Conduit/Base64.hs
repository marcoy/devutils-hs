module DevUtils.Cmd.Internal.Conduit.Base64
  ( b64EncodeC
  , b64DecodeC
  ) where


import           Conduit
import           Control.Exception      (Exception)
import           Control.Monad.Extra    (unless)
import qualified Data.Base64.Types      as B64
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Typeable          (Typeable)


newtype MalformedBase64 = MalformedBase64 Text
                          deriving (Show, Typeable)
                          deriving anyclass (Exception)

b64EncodeC :: (Monad m) => ConduitM ByteString ByteString m ()
b64EncodeC = go BS.empty
  where
    go !acc = do
      mchunk <- await
      case mchunk of
        Nothing    -> unless (BS.null acc) $ yield (B64.extractBase64 $ B64.encodeBase64' acc)
        Just chunk -> do
          let combined   = acc <> chunk
              len        = BS.length combined
              alignedLen = len - (len `mod` 3)
              (toEncode, rest) = BS.splitAt alignedLen combined
          unless (BS.null toEncode) $
            yield (B64.extractBase64 $ B64.encodeBase64' toEncode)
          go rest

b64DecodeC :: (MonadThrow m) => ConduitM ByteString ByteString m ()
b64DecodeC = go BS.empty
  where
    go !acc = do
      mchunk <- await
      case mchunk of
        Nothing    -> unless (BS.null acc) $ d64 acc >>= yield
        Just chunk -> do
          let combined   = acc <> chunk
              len        = BS.length combined
              alignedLen = len - (len `mod` 4)
              (toDecode, rest) = BS.splitAt alignedLen combined
          unless (BS.null toDecode) $ d64 toDecode >>= yield
          go rest
    d64 (B64.decodeBase64Untyped -> Left err) = throwM $ MalformedBase64 err
    d64 (B64.decodeBase64Untyped -> Right bs) = pure bs
