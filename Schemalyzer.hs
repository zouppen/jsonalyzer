{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
import GHC.Exts (sortWith)

-- Extra
import qualified Data.Text.IO as T
import System.IO

data Node = ArrayNode | ObjectNode T.Text deriving (Show, Eq, Ord)

type Path = [Node]
type PathValue = (Path, Value)
type Mashup = M.Map Path (HMS.HashMap Value Int)

loadJson :: FilePath -> IO Value
loadJson f = B.readFile f >>= either error pure . eitherDecode

-- |Shorthand for analyzing JSON structure
analyzeRoot = analyze []

-- |Extract path and its values recursively from given JSON value
analyze :: Path -> Value -> [PathValue]
analyze p v = case v of
  Object x -> HMS.foldrWithKey objFlattern me x
  Array x  -> V.foldr arrFlattern me x
  _        -> me
  where
    me = [(p,v)]
    objFlattern :: T.Text -> Value -> [PathValue] -> [PathValue]
    objFlattern k v a = a ++ analyze (ObjectNode k:p) v
    arrFlattern :: Value -> [PathValue] -> [PathValue]
    arrFlattern v a = a ++ analyze (ArrayNode:p) v

-- |Print path in user-friendly format
formatPath :: Path -> T.Text
formatPath p = foldr f T.empty p
  where f ArrayNode a = a `T.append` "[]"
        f (ObjectNode t) a = a `T.append` "." `T.append` t

mash :: [PathValue] -> Mashup
mash xs = foldl buildOuter M.empty xs
  where buildOuter a (p,v) = M.insertWith sumKeys p (singletonValue v) a
        sumKeys = HMS.unionWith (+)
        singletonValue v = HMS.singleton v 1

formatMash :: Mashup -> [T.Text]
formatMash m = concatMap formatCounts $ M.toList m
  where formatCounts (p,m) = pathText : (clipList 15 cont $ map (TL.toStrict . fromCount) values)
          where pathText = T.concat [formatPath p, ": ", total, " total, ", distinctness]
                spaces = "        "
                cont   = "        ..."
                fromCount (val,n) = clipLine 80 $ TL.concat
                                   [spaces, TL.pack $ show n, "x ", TL.decodeUtf8 $ encode val]
                values = sortWith (negate.snd) $ HMS.toList m
                distinct = toText $ HMS.size m
                total = toText $ sum $ HMS.elems m
                distinctness = if total == distinct
                               then "all distinct"
                               else distinct `T.append` " distinct"

toText :: (Show a) => a -> T.Text 
toText = T.pack . show

clipLine n t = if TL.length t > n
               then TL.take (n-3) t `TL.append` "..."
               else t

clipList n trail xs = if length xs > n
           then take (n-1) xs ++ [trail]
           else xs

-- |Analyzes JSON file and outputs text file.
testAnalyze inFile outFile = do
  js <- analyzeRoot <$> loadJson inFile
  withFile outFile WriteMode $ \h -> mapM_ (T.hPutStrLn h) $ formatMash $ mash js
