module DevUtils.Cmd.Base64
  ( base64Handler
  ) where


import           Conduit                              (decodeUtf8C, mapMC,
                                                       runConduit, sinkNull,
                                                       (.|))
import           Control.Monad.Catch                  (MonadThrow)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Resource         (MonadResource)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import           DevUtils.Cmd.Internal.Conduit.Base64 (b64DecodeC, b64EncodeC)
import           DevUtils.Cmd.Internal.Conduit.IO     (fpInputSource)
import           DevUtils.Opts                        (Base64Options (..))


base64Handler :: forall m. (MonadResource m, MonadThrow m) => Base64Options -> m ()
base64Handler opts = do
  let source = fpInputSource opts.inputFp
      op     = if opts.decodeMode then b64DecodeC else b64EncodeC
  runConduit $ source
            .| op
            .| decodeUtf8C
            .| mapMC (liftIO . TIO.putStr)
            .| sinkNull
