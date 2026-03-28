
module DevUtils.Cmd.Internal.Conduit.IO
  (fpInputSource) where


import           Conduit                      (ConduitT, sourceFile, stdinC)
import           Control.Monad.Extra          (ifM)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString              (ByteString)
import           System.Directory.Extra       (doesPathExist)


fpInputSource :: (MonadResource m) => Maybe FilePath -> ConduitT () ByteString m ()
fpInputSource = \case
  Nothing  -> stdinC
  Just "-" -> stdinC
  Just fp  -> ifM (liftIO $ doesPathExist fp)
                (sourceFile fp)
                (error $ fp <> " not found")
