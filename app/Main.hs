import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           DevUtils.AppM   (runDevUApp)
import           DevUtils.Cmd
import           DevUtils.Opts   (DevUCommands (..), DevUOptions (..),
                                  UUIDOptions (..), execArgsParser)
import           System.IO.Extra (BufferMode (NoBuffering), hSetBuffering,
                                  stdout)
import           TextShow        (TextShow (showt))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- execArgsParser

  case args.cmd of
    (UUID opts) -> runDevUApp $ generateUUID opts
    ULID        -> error "Not Implemented"
