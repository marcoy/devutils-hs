{-# LANGUAGE UndecidableInstances #-}
module DevUtils.AppM (
  DevUAppT (..)
, DevUApp

, runDevUApp
) where


import           Conduit                      (PrimMonad)
import           Control.Monad.Catch          (MonadCatch, MonadMask,
                                               MonadThrow)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans          (MonadTrans (lift))
import           Control.Monad.Trans.Resource (MonadResource, ResourceT,
                                               runResourceT)
import           GHC.Generics                 (Generic)
import           UnliftIO                     (MonadUnliftIO)


newtype DevUAppT m a = DevUAppT { app :: ResourceT m a }
                       deriving stock (Generic)
                       deriving newtype (
                           Functor
                         , Applicative
                         , Monad
                         , MonadCatch
                         , MonadIO
                         , MonadMask
                         , MonadResource
                         , MonadThrow
                         , MonadUnliftIO
                         , PrimMonad
                         )

instance MonadTrans DevUAppT where
  lift = DevUAppT . lift

type DevUApp = DevUAppT IO

runDevUApp :: DevUApp a -> IO a
runDevUApp devutils = runResourceT devutils.app
