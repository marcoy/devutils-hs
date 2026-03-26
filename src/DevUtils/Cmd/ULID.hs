module DevUtils.Cmd.ULID
  (generateULID) where


import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Maybe              (maybe)
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           Data.ULID               (ULID (..), getULID)
import           Data.ULID.Random        (getULIDRandom, mkULIDRandom)
import           Data.ULID.TimeStamp     (getULIDTimeStamp)
import           DevUtils.Opts           (ULIDOptions (..))
import           System.Random           (mkStdGen64)


generateULID :: (MonadIO m) => ULIDOptions -> m ()
generateULID opts = do
  ts <- liftIO getULIDTimeStamp
  rd <- liftIO $ maybe getULIDRandom mkRdm opts.seed
  let ulid = transform . T.pack . show $ ULID ts rd
  liftIO $ TIO.putStr ulid
  pure ()

  where
    mkRdm seed = pure . fst . mkULIDRandom $ mkStdGen64 seed
    transform = if opts.upperCase then id else T.toLower
