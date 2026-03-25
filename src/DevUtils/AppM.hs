module DevUtils.AppM (
  DevUAppT (..)
, DevUApp

, runDevUApp
) where


import           Control.Monad.Catch    (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (MonadTrans)
import           GHC.Generics           (Generic)
import           UnliftIO               (MonadUnliftIO)


newtype DevUAppT m a = DevUAppT { app :: m a }
                       deriving stock (Generic)
                       deriving newtype (
                           Functor
                         , Applicative
                         , Monad
                         , MonadThrow
                         , MonadCatch
                         , MonadMask
                         , MonadIO
                         , MonadUnliftIO
                         )

type DevUApp = DevUAppT IO

runDevUApp :: DevUApp a -> IO a
runDevUApp devutils = devutils.app
