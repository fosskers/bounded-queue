module Main where

import Criterion.Main
import Data.Foldable (foldl')
import Data.Queue.Bounded

---

main :: IO ()
main = defaultMain
  [ bgroup "Consing"
    [ bench "Lazy List - Take" $
        nf (foldl' (flip consBQLT) (BQListTake [] 10)) ([1..100] :: [Int])
    , bench "Lazy List - Count" $
        nf (foldl' (flip consBQLC) (BQListCount [] 10 0)) ([1..100] :: [Int])
    , bench "Strict List - Take" $
        nf (foldl' (flip consBQST) (BQStrictTake mempty 10)) ([1..100] :: [Int])
    , bench "Strict List - Count" $
        nf (foldl' (flip consBQSC) (BQStrictCount mempty 10 0)) ([1..100] :: [Int])
    ]
  ]
