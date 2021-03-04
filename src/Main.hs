import Control.Monad (replicateM_)
import Data.Functor ((<&>))
import Data.Text.IO (putStrLn, readFile)
import Data.Time (diffTimeToPicoseconds, getCurrentTime, utctDayTime)
import Markov (chainToText, distrsToChain, textToDistrs)
import Random (seed)
import System.Environment (getArgs)
import Prelude hiding (putStrLn, readFile)

main :: IO ()
main = do
  t <- fromInteger . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
  seed t 0
  distrs <- (getArgs >>= readFile . head) <&> textToDistrs
  replicateM_ 10 (putStrLn . chainToText =<< distrsToChain distrs)
