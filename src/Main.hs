import Data.Text (Text, lines, words)
import Data.Text.IO (readFile)
import System.Environment (getArgs)
import Prelude hiding (Word, lines, readFile, words)

-- import Random (getRandom, setSeed)

-- main :: IO ()
-- main = setSeed 0 0 >> getRandom >>= print

data Token
  = Start
  | Word Text
  | End
  deriving (Show)

f :: Text -> [[Token]]
f = map (\x -> Start : map Word (words x) ++ [End]) . lines

main :: IO ()
main = print . head . f =<< readFile . head =<< getArgs
