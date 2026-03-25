module DevUtils.Cmd.UUID (generateUUID) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.UUID              as U
import qualified Data.UUID.V4           as U
import           DevUtils.Opts          (UUIDOptions (..))


generateUUID :: (MonadIO m) => UUIDOptions -> m ()
generateUUID opts = do
  let transform = if opts.upperCase then T.toUpper else id
  uuid <- liftIO $ transform . U.toText <$> U.nextRandom
  liftIO $ TIO.putStr uuid
