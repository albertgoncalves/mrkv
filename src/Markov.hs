module Markov where

import Control.Exception (assert)
import Data.Map
  ( Map,
    empty,
    fromAscList,
    insertWith,
    lookupGT,
    singleton,
    toList,
    unionWith,
    valid,
    (!),
  )
import Data.Maybe (mapMaybe)
import Data.Text (Text, lines, unwords, words)
import Data.Word (Word32)
import Random (randomBounded)
import Prelude hiding (Word, lines, unwords, words)

data Token
  = Start
  | Word !Text
  | End
  deriving (Eq, Ord)

type Table = Map Token Word32

type Distr = (Word32, Map Word32 Token)

textToTokens :: Text -> [[Token]]
textToTokens = map ((Start :) . (++ [End]) . map Word . words) . lines

combineTables :: Table -> Table -> Table
combineTables = unionWith (+)

insertTokens :: (Token, Token) -> Map Token Table -> Map Token Table
insertTokens (k, v) = insertWith combineTables k (singleton v 1)

updateTables :: Map Token Table -> [Token] -> Map Token Table
updateTables m ts = foldr insertTokens m $ zip ts $ tail ts

tableToDistr :: Table -> Distr
tableToDistr t = assert (all (0 <) ns && valid m) (sum ns, m)
  where
    (ws, ns) = unzip $ toList t
    m = fromAscList $ zip (scanl1 (+) ns) ws

makeDistrs :: [[Token]] -> Map Token Distr
makeDistrs =
  fmap tableToDistr
    . foldr (unionWith combineTables . updateTables empty) empty

distrsToChain :: Map Token Distr -> IO [Token]
distrsToChain m = f Start
  where
    f :: Token -> IO [Token]
    f t = (t :) <$> loop t

    loop :: Token -> IO [Token]
    loop End = return []
    loop t =
      randomBounded n >>= \i -> case lookupGT i m' of
        Just (_, t') -> f t'
        Nothing -> undefined
      where
        (n, m') = m ! t

textToDistrs :: Text -> Map Token Distr
textToDistrs = makeDistrs . textToTokens

chainToText :: [Token] -> Text
chainToText = unwords . mapMaybe f
  where
    f :: Token -> Maybe Text
    f (Word x) = Just x
    f _ = Nothing
