import Control.Monad (replicateM_)
import Data.Functor ((<&>))
import Data.Text.IO (readFile)
import Data.Time (diffTimeToPicoseconds, getCurrentTime, utctDayTime)
import Markov (chainToText, distrsToChain, seed, textToDistrs)
import System.Environment (getArgs)
import Prelude hiding (readFile)

main :: IO ()
main = do
  t <- fromInteger . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
  seed t 0
  distrs <- (getArgs >>= readFile . head) <&> textToDistrs
  replicateM_ 10 (print . chainToText =<< distrsToChain distrs)
