{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE BlockArguments #-}
module Benchmark where

import Criterion
import Criterion.Main
import Data.List (intercalate, intersperse, transpose)
import Control.DeepSeq (deepseq)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
  let exampleString :: String = concat . take 10 . repeat $ [ c | c <- ['a' .. 'z'] <> ['1' .. '9'] ]
      exampleText :: T.Text = T.pack exampleString
      exampleMap :: Map.Map T.Text Int = Map.fromList [(T.pack $ show i, i) | i <- [1..1000]]
      exampleHashMap :: HashMap.HashMap T.Text Int = HashMap.fromList [(T.pack $ show i, i) | i <- [1..1000]]
  () <- pure (deepseq exampleString ())
  () <- pure (deepseq exampleText ())
  () <- pure (deepseq exampleMap ())
  () <- pure (deepseq exampleHashMap ())
  defaultMain
    [ bgroup "strings"
      [ bgroup "last"
        [ bench "Text" (nf T.last exampleText)
        , bench "String" (nf last exampleString)
        ]
      , bench "pack" (nf T.pack exampleString)
      , bench "unpack" (nf T.unpack exampleText)
      , bgroup "singleton" 
        [ bench "Text" (nf T.singleton 'x')
        , bench "String" (nf (pure @[]) 'x')
        ]
      , bgroup "snoc"
        [ bench "Text" (nf (flip T.snoc 'x') exampleText)
        , bench "String" (nf (<> ['x']) exampleString)
        ]
      , bgroup "head"
        [ bench "Text" (nf T.head exampleText)
        , bench "String" (nf head exampleString)
        ]
      , bgroup "append"
        [ bench "Text" (nf (T.append exampleText) exampleText)
        , bench "String" (nf ((<>) exampleString) exampleString)
        ]
      , bgroup "tail"
        [ bench "Text" (nf T.tail exampleText)
        , bench "String" (nf tail exampleString)
        ]
      , bgroup "init"
        [ bench "Text" (nf T.init exampleText)
        , bench "String" (nf init exampleString)
        ]
      , bgroup "length"
        [ bench "Text" (nf T.length exampleText)
        , bench "String" (nf length exampleString)
        ]
      , bgroup "map"
        [ bench "Text" (nf (T.map (\c -> if c == '\n' then ' ' else c)) exampleText)
        , bench "String" (nf (map (\c -> if c == '\n' then ' ' else c)) exampleString)
        ]
      , bgroup "intercalate"
        [ bench "Text" (nf (T.intercalate ",") [exampleText | i <- [1..10]])
        , bench "String" (nf (intercalate ",") [exampleString | i <- [1..10]])
        ]
      , bgroup "intersperse"
        [ bench "Text" (nf (T.intersperse ',') exampleText)
        , bench "String" (nf (intersperse ',') exampleString)
        ]
--      , bgroup "transpose"
--        [ bench "Text" (nf T.transpose [exampleText | i <- [1..10]])
--        , bench "String" (nf transpose [exampleString | i <- [1..10]])
--        ]
      , bgroup "reverse"
        [ bench "Text" (nf T.reverse exampleText)
        , bench "String" (nf reverse exampleString)
        ]
      , bgroup "replace"
        [ bench "Text" (nf (T.replace "a" "yak") exampleText)
        ]
      ]
    , bgroup "strict-maps"
      [ bgroup "insert"
        [ bench "Map" (nf (Map.insert "1001" 1001) exampleMap)
        , bench "HashMap" (nf (HashMap.insert "1001" 1001) exampleHashMap)
        ]
      , bgroup "lookup"
        [ bench "Map" (nf (Map.lookup "500") exampleMap)
        , bench "HashMap" (nf (HashMap.lookup "500") exampleHashMap)
        ]
      , bgroup "member"
        [ bench "Map" (nf (Map.member "500") exampleMap)
        , bench "HashMap" (nf (HashMap.member "500") exampleHashMap)
        ]
      , bgroup "insertWith"
        [ bench "Map" (nf (Map.insertWith (+) "500" 100) exampleMap)
        , bench "HashMap" (nf (HashMap.insertWith (+) "500" 100) exampleHashMap)
        ]
      , bgroup "delete"
        [ bench "Map" (nf (Map.delete "500") exampleMap)
        , bench "HashMap" (nf (HashMap.delete "500") exampleHashMap)
        ]
      , bgroup "adjust"
        [ bench "Map" (nf (Map.adjust (+ 100) "500") exampleMap)
        , bench "HashMap" (nf (HashMap.adjust (+ 100) "500") exampleHashMap)
        ]
      , bgroup "foldl'"
        [ bench "Map" (nf (Map.foldl' (+) 0) exampleMap)
        , bench "HashMap" (nf (HashMap.foldl' (+) 0) exampleHashMap)
        ]
      , bgroup "foldr"
        [ bench "Map" (nf (Map.foldr (+) 0) exampleMap)
        , bench "HashMap" (nf (HashMap.foldr (+) 0) exampleHashMap)
        ]
      , bgroup "elems"
        [ bench "Map" (nf Map.elems exampleMap)
        , bench "HashMap" (nf HashMap.elems exampleHashMap)
        ]
      , bgroup "filter"
        [ bench "Map" (nf (Map.filter even) exampleMap)
        , bench "HashMap" (nf (HashMap.filter even) exampleHashMap)
        ]
      ]
    ]
