{-# LANGUAGE OverloadedLists #-}

module Main where

import Criterion.Main
import Data.Foldable (foldl')
import Data.Queue.Bounded

---

input :: [Int]
input = [1..100000]

main :: IO ()
main = defaultMain
  [ bgroup "Consing"
    [ -- bench "Lazy List - Take" $
        -- nf (foldl' (flip consBQLT) (BQListTake [] 10)) input
    -- , bench "Lazy List - Count" $
    --     nf (foldl' (flip consBQLC) (BQListCount [] 10 0)) input
    -- , bench "Strict List - Take" $
    --     nf (foldl' (flip consBQST) (BQStrictTake mempty 10)) input
    -- , bench "Strict List - Count" $
    --     nf (foldl' (flip consBQSC) (BQStrictCount mempty 10 0)) input
      bench "Seq - Count" $
        nf (foldl' (flip consSeq) (BQSeq mempty 10 0)) input
    , bench "Mutable Vector" $
        nf (foldl' (flip consVec) (BQVec mempty 10 0)) input
    ]
  -- , bgroup "Average"
  --   [ bench "Seq" $ nf avgSeq (BQSeq [1..1000] 1000 1000)
  --   , bench "Vec" $ nf avgVec (BQVec [1..1000] 1000 1000)
  --   ]
  ]
