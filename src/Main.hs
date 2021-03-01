import Data.Map.Strict
  ( Map,
    empty,
    fromList,
    insertWith,
    lookupGT,
    singleton,
    toList,
    unionWith,
    (!),
  )
import Data.Text (Text, lines, words)
import Data.Text.IO (readFile)
import Data.Word (Word32)
import Random (randomBounded, seed)
import System.Environment (getArgs)
import Prelude hiding (Word, lines, readFile, words)

data Token
  = Start
  | Word Text
  | End
  deriving (Eq, Ord, Show)

type Table = Map Token Word32

type Distr = (Word32, Map Word32 Token)

textToTokens :: Text -> [[Token]]
textToTokens = map (\x -> Start : map Word (words x) ++ [End]) . lines

combineTables :: Table -> Table -> Table
combineTables = unionWith (+)

insertPair :: Token -> Token -> Map Token Table -> Map Token Table
insertPair k v = insertWith combineTables k (singleton v 1)

updateChain :: Map Token Table -> [Token] -> Map Token Table
updateChain m xs = foldr (uncurry insertPair) m $ zip xs $ tail xs

makeChain :: [[Token]] -> Map Token Table
makeChain = foldr (unionWith combineTables . updateChain empty) empty

tableToDistr :: Table -> Distr
tableToDistr t = (n, fromList $ zip ns' ws)
  where
    (ws, ns) = unzip $ toList t
    ns' = scanl1 (+) ns
    n = sum ns

chainToDistrs :: Map Token Table -> Map Token Distr
chainToDistrs = fmap tableToDistr

nextToken :: Map Token Distr -> Token -> IO Token
nextToken m t = do
  i <- randomBounded n
  return $ case lookupGT i m' of
    Just (_, t') -> t'
    Nothing -> undefined
  where
    (n, m') = m ! t

generate :: Map Token Distr -> IO [Token]
generate m = (Start :) <$> f m Start
  where
    f _ End = return []
    f m' t = do
      t' <- nextToken m' t
      (t' :) <$> f m' t'

main :: IO ()
main = do
  seed 0 0
  filename <- head <$> getArgs
  text <- readFile filename
  let m = chainToDistrs $ makeChain $ textToTokens text
  print =<< generate m
