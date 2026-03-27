{-# LANGUAGE QuasiQuotes #-}
module DevUtils.Cmd.Base64
  ( base64Handler
  ) where


import           Conduit                              (ConduitM, decodeUtf8C,
                                                       mapMC, runConduit,
                                                       sinkNull, sourceFile,
                                                       stdinC, (.|))
import           Control.Monad.Catch                  (MonadThrow (..))
import           Control.Monad.Extra                  (ifM)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Resource         (MonadResource)
import           Data.ByteString                      (ByteString)
import           Data.String.Interpolate              (i)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import           DevUtils.Cmd.Internal.Conduit.Base64 (b64DecodeC, b64EncodeC)
import           DevUtils.Opts                        (Base64Options (..))
import           System.Directory.Extra               (doesPathExist)


base64Handler :: forall m. (MonadResource m, MonadThrow m) => Base64Options -> m ()
base64Handler opts = do
  let source = inputSource opts.inputFp
      op     = if opts.decodeMode then b64DecodeC else b64EncodeC
  runConduit $ source
            .| op
            .| decodeUtf8C
            .| mapMC (liftIO . TIO.putStr)
            .| sinkNull


inputSource :: (MonadResource m) => Maybe FilePath -> ConduitM () ByteString m ()
inputSource = \case
  Nothing  -> stdinC
  Just "-" -> stdinC
  Just fp  -> ifM (liftIO $ doesPathExist fp)
                (sourceFile fp)
                (error [i|'#{fp}' not found|])
