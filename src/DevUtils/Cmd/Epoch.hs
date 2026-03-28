{-# LANGUAGE QuasiQuotes #-}
module DevUtils.Cmd.Epoch (epochHandler) where

import           Control.Monad.Extra           (when)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Int                      (Int64)
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.String.Interpolate       (i)
import qualified Data.Text.IO                  as TIO
import           Data.Time                     (getCurrentTimeZone,
                                                utcToLocalTime)
import           Data.Time.Clock.POSIX         (getPOSIXTime,
                                                posixSecondsToUTCTime)
import           Data.Time.Format.ISO8601      (iso8601Show)
import           DevUtils.Data.Epoch.EpochUnit (EpochUnit (EpochMillis, EpochSeconds))
import           DevUtils.Opts                 (EpochOptions (..))


epochHandler :: (MonadIO m) => EpochOptions -> m ()
epochHandler opts = do
  case opts.epochTimestamp of
    Nothing -> displayCurrent
    Just ts -> displayTimestamp ts opts.epochUnit


displayCurrent :: (MonadIO m) => m ()
displayCurrent = do
  ts <- liftIO getPOSIXTime
  tz <- liftIO getCurrentTimeZone
  let seconds   = floor ts :: Int64
      millis    = floor (ts * 1000) :: Int64
      utcTime   = posixSecondsToUTCTime ts
      localTime = utcToLocalTime tz utcTime

  liftIO $ TIO.putStrLn [i|Epoch seconds: #{seconds}|]
  liftIO $ TIO.putStrLn [i|Epoch millis : #{millis}|]
  liftIO $ TIO.putStrLn [i|UTC Time     : #{iso8601Show utcTime}|]
  liftIO $ TIO.putStrLn [i|Local Time   : #{iso8601Show localTime}|]


displayTimestamp :: (MonadIO m) => Int64 -> Maybe EpochUnit -> m ()
displayTimestamp ts epochUnit' = do
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz utcTime

  when (isNothing epochUnit') $ liftIO $ TIO.putStrLn [i|Assuming unit to be #{epochUnit}|]
  liftIO $ TIO.putStrLn [i|UTC Time     : #{iso8601Show utcTime}|]
  liftIO $ TIO.putStrLn [i|Local Time   : #{iso8601Show localTime}|]

  where
    epochUnit :: EpochUnit
    epochUnit = fromMaybe (unitHeuristic ts) epochUnit'

    unitHeuristic :: Int64 -> EpochUnit
    unitHeuristic ts
      | ts >= 10^(12 :: Int) = EpochMillis
      | otherwise            = EpochSeconds

    utcTime = case epochUnit of
      EpochSeconds -> posixSecondsToUTCTime (fromIntegral ts)
      EpochMillis  -> posixSecondsToUTCTime (fromIntegral ts / 1000)
