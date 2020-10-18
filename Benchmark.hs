{-# LANGUAGE PackageImports #-}
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
import qualified "text" Data.Text as T
import qualified "text-utf8" Data.Text as T8
import qualified Data.Text.Short as TS
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
  let exampleString :: String = concat . take 10 . repeat $ [ c | c <- ['a' .. 'z'] <> ['1' .. '9'] ]
      exampleText :: T.Text = T.pack exampleString
      exampleText8 :: T8.Text = T8.pack exampleString
      exampleShortText :: TS.ShortText = TS.pack exampleString
      exampleMap :: Map.Map T.Text Int = Map.fromList [(T.pack $ show i, i) | i <- [1..1000]]
      exampleHashMap :: HashMap.HashMap T.Text Int = HashMap.fromList [(T.pack $ show i, i) | i <- [1..1000]]
  () <- pure (deepseq exampleString ())
  () <- pure (deepseq exampleText ())
  () <- pure (deepseq exampleText8 ())
  () <- pure (deepseq exampleShortText ())
  () <- pure (deepseq exampleMap ())
  () <- pure (deepseq exampleHashMap ())
  defaultMain
    [ bgroup "strings"
      [ bgroup "last"
        [ bench "Text-Utf16" (nf T.last exampleText)
        , bench "Text-Utf8" (nf T8.last exampleText8)
        , bench "String" (nf last exampleString)
        ]
      , bgroup "pack"
        [ bench "Text-Utf16" (nf T.pack exampleString)
        , bench "Text-Utf8" (nf T8.pack exampleString)
        ]
      , bgroup "unpack"
        [ bench "Text-Utf16" (nf T.unpack exampleText)
        , bench "Text-Utf8" (nf T8.unpack exampleText8)
        ]
      , bgroup "singleton" 
        [ bench "Text-Utf16" (nf T.singleton 'x')
        , bench "Text-Utf8" (nf T8.singleton 'x')
        , bench "ShortText" (nf TS.singleton 'x')
        , bench "String" (nf (pure @[]) 'x')
        ]
      , bgroup "snoc"
        [ bench "Text-Utf16" (nf (flip T.snoc 'x') exampleText)
        , bench "Text-Utf8" (nf (flip T8.snoc 'x') exampleText8)
        , bench "ShortText" (nf (flip TS.snoc 'x') exampleShortText)
        , bench "String" (nf (<> ['x']) exampleString)
        ]
      , bgroup "head"
        [ bench "Text-Utf16" (nf T.head exampleText)
        , bench "Text-Utf8" (nf T8.head exampleText8)
        , bench "String" (nf head exampleString)
        ]
      , bgroup "append"
        [ bench "Text-Utf16" (nf (T.append exampleText) exampleText)
        , bench "Text-Utf8" (nf (T8.append exampleText8) exampleText8)
        , bench "ShortText" (nf (TS.append exampleShortText) exampleShortText)
        , bench "String" (nf ((<>) exampleString) exampleString)
        ]
      , bgroup "tail"
        [ bench "Text-Utf16" (nf T.tail exampleText)
        , bench "Text-Utf8" (nf T8.tail exampleText8)
        , bench "String" (nf tail exampleString)
        ]
      , bgroup "init"
        [ bench "Text-Utf16" (nf T.init exampleText)
        , bench "Text-Utf8" (nf T8.init exampleText8)
        , bench "String" (nf init exampleString)
        ]
      , bgroup "length"
        [ bench "Text-Utf16" (nf T.length exampleText)
        , bench "Text-Utf8" (nf T8.length exampleText8)
        , bench "ShortText" (nf TS.length exampleShortText)
        , bench "String" (nf length exampleString)
        ]
      , bgroup "map"
        [ bench "Text-Utf16" (nf (T.map (\c -> if c == '\n' then ' ' else c)) exampleText)
        , bench "Text-Utf8" (nf (T8.map (\c -> if c == '\n' then ' ' else c)) exampleText8)
        , bench "String" (nf (map (\c -> if c == '\n' then ' ' else c)) exampleString)
        ]
      , bgroup "intercalate"
        [ bench "Text-Utf16" (nf (T.intercalate ",") [exampleText | i <- [1..10]])
        , bench "Text-Utf8" (nf (T8.intercalate ",") [exampleText8 | i <- [1..10]])
        , bench "ShortText" (nf (TS.intercalate ",") [exampleShortText | i <- [1..10]])
        , bench "String" (nf (intercalate ",") [exampleString | i <- [1..10]])
        ]
      , bgroup "intersperse"
        [ bench "Text-Utf16" (nf (T.intersperse ',') exampleText)
        , bench "Text-Utf8" (nf (T8.intersperse ',') exampleText8)
        , bench "ShortText" (nf (TS.intersperse ',') exampleShortText)
        , bench "String" (nf (intersperse ',') exampleString)
        ]
--      , bgroup "transpose"
--        [ bench "Text-Utf16" (nf T.transpose [exampleText | i <- [1..10]])
--        , bench "Text-Utf8" (nf T8.transpose [exampleText8 | i <- [1..10]])
--        , bench "String" (nf transpose [exampleString | i <- [1..10]])
--        ]
      , bgroup "reverse"
        [ bench "Text-Utf16" (nf T.reverse exampleText)
        , bench "Text-Utf8" (nf T8.reverse exampleText8)
        , bench "ShortText" (nf TS.reverse exampleShortText)
        , bench "String" (nf reverse exampleString)
        ]
      , bgroup "replace"
        [ bench "Text-Utf16" (nf (T.replace "a" "yak") exampleText)
        , bench "Text-Utf8" (nf (T8.replace "a" "yak") exampleText8)
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
