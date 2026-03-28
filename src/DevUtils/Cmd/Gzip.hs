module DevUtils.Cmd.Gzip
  ( gzipHandler
  ) where


import           Conduit                              (ConduitT, PrimMonad,
                                                       decodeUtf8C, mapMC,
                                                       runConduit, sinkNull,
                                                       stdinC, stdoutC, (.|))
import           Control.Monad.Catch                  (MonadThrow)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Resource         (MonadResource)
import           Data.ByteString                      (ByteString)
import           Data.Conduit.Zlib                    (gzip, ungzip)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import           DevUtils.Cmd.Internal.Conduit.Base64 (b64DecodeC, b64EncodeC)
import           DevUtils.Cmd.Internal.Conduit.IO     (fpInputSource)
import           DevUtils.Opts                        (GzipOptions (..))


gzipHandler :: forall m. (MonadResource m, MonadThrow m, PrimMonad m) => GzipOptions -> m ()
gzipHandler opts = do
  let source = fpInputSource opts.inputFp
  runConduit $ source
            .| opC
            .| stdoutC

  where
    opC = if opts.decompressMode then decompressC else compressC

    compressC :: ConduitT ByteString ByteString m ()
    compressC = if opts.base64 then gzip .| b64EncodeC else gzip

    decompressC :: ConduitT ByteString ByteString m ()
    decompressC = if opts.base64 then b64DecodeC .| ungzip else ungzip
