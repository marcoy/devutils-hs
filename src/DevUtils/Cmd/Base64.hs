{-# LANGUAGE QuasiQuotes #-}
module DevUtils.Cmd.Base64
  ( base64Handler

  , MalformedBase64 (..)
  ) where


import           Conduit                      (ConduitM, await, decodeUtf8C,
                                               mapMC, runConduit, sinkNull,
                                               sourceFile, stdinC, yield, (.|))
import           Control.Exception            (Exception)
import           Control.Monad.Catch          (MonadThrow (..))
import           Control.Monad.Extra          (ifM, unless)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Base64.Types            as B64
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as B64
import           Data.String.Interpolate      (i)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import           Data.Typeable                (Typeable)
import           DevUtils.Opts                (Base64Options (..))
import           System.Directory.Extra       (doesPathExist)



newtype MalformedBase64 = MalformedBase64 Text
                          deriving (Show, Typeable)
                          deriving anyclass (Exception)


base64Handler :: forall m. (MonadResource m, MonadThrow m) => Base64Options -> m ()
base64Handler opts = do
  let source = inputSource opts.inputFp
      op     = if opts.decodeMode then b64DecodeC else b64EncodeC
  runConduit $ source
            .| op
            .| mapMC (liftIO . TIO.putStr)
            .| sinkNull


inputSource :: (MonadResource m) => Maybe FilePath -> ConduitM () ByteString m ()
inputSource = \case
  Nothing  -> stdinC
  Just "-" -> stdinC
  Just fp  -> ifM (liftIO $ doesPathExist fp)
                (sourceFile fp)
                (error [i|'#{fp}' not found|])

b64EncodeC :: (Monad m) => ConduitM ByteString Text m ()
b64EncodeC = go BS.empty
  where
    go !acc = do
      mchunk <- await
      case mchunk of
        Nothing    -> unless (BS.null acc) $ yield (B64.extractBase64 $ B64.encodeBase64 acc)
        Just chunk -> do
          let combined   = acc <> chunk
              len        = BS.length combined
              alignedLen = len - (len `mod` 3)
              (toEncode, rest) = BS.splitAt alignedLen combined
          unless (BS.null toEncode) $
            yield (B64.extractBase64 $ B64.encodeBase64 toEncode)
          go rest

b64DecodeC :: (MonadThrow m) => ConduitM ByteString Text m ()
b64DecodeC = go BS.empty .| decodeUtf8C
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
