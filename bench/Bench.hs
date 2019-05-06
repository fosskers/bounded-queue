{-# LANGUAGE OverloadedLists #-}

module Main where

import           Criterion.Main
import           Data.Foldable (foldl')
import qualified Data.Queue.Bounded as BQ

---

input :: [Int]
input = [1..10000]

main :: IO ()
main = defaultMain
  [ bgroup "Consing"
    [bench "Seq - Count" $ nf (foldl' (flip BQ.cons) (BQ.empty 10)) input
    ]
  , bgroup "Average"
    [ -- bench "Seq" $ nf avgSeq (BQ.empty [1..1000] 1000 1000)
    ]
  ]
