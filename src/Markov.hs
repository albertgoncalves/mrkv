module Markov where

import Control.Exception (assert)
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
import Data.Maybe (mapMaybe)
import Data.Text (Text, lines, unwords, words)
import Data.Word (Word32)
import Random (randomBounded)
import Prelude hiding (Word, lines, unwords, words)

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

insertTokens :: Token -> Token -> Map Token Table -> Map Token Table
insertTokens a b = insertWith combineTables a (singleton b 1)

updateTables :: Map Token Table -> [Token] -> Map Token Table
updateTables m xs = foldr (uncurry insertTokens) m $ zip xs $ tail xs

tableToDistr :: Table -> Distr
tableToDistr t =
  assert (all (0 <) ns) (sum ns, fromList $ zip (scanl1 (+) ns) ws)
  where
    (ws, ns) = unzip $ toList t

makeDistrs :: [[Token]] -> Map Token Distr
makeDistrs =
  fmap tableToDistr
    . foldr (unionWith combineTables . updateTables empty) empty

distrsToChain :: Map Token Distr -> IO [Token]
distrsToChain m = f Start
  where
    f t = (t :) <$> loop t

    loop :: Token -> IO [Token]
    loop End = return []
    loop t = do
      i <- randomBounded n
      case lookupGT i m' of
        Just (_, t') -> f t'
        Nothing -> undefined
      where
        (n, m') = m ! t

textToDistrs :: Text -> Map Token Distr
textToDistrs = makeDistrs . textToTokens

chainToText :: [Token] -> Text
chainToText = unwords . mapMaybe f
  where
    f (Word x) = Just x
    f _ = Nothing
