{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module    : Data.Queue.Bounded
-- Copyright : (c) Kadena LLC, 2019
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@kadena.io>

module Data.Queue.Bounded where

import           Control.DeepSeq (NFData)
import           Control.Monad.ST (runST)
import           Data.Foldable (foldl')
import           Data.Ratio ((%))
import           Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           GHC.Generics (Generic)
import qualified StrictList as SL

---

data BQListTake a = BQListTake
  { _bqlt      :: ![a]
  , _bqltLimit :: {-# UNPACK #-} !Int }
  deriving (Generic, NFData)

consBQLT :: a -> BQListTake a -> BQListTake a
consBQLT a (BQListTake q l) = BQListTake (take l $ a : q) l

---

data BQListCount a = BQListCount
  { _bqlc      :: ![a]
  , _bqlcLimit :: {-# UNPACK #-} !Int
  , _bqlcSize  :: {-# UNPACK #-} !Int }
  deriving (Generic, NFData)

consBQLC :: a -> BQListCount a -> BQListCount a
consBQLC a (BQListCount q l s)
  | s == l = BQListCount (a : init q) l s
  | otherwise = BQListCount (a : q) l (succ s)

---

data BQStrictTake a = BQStrictTake
  { _bqst      :: !(SL.List a)
  , _bqstLimit :: {-# UNPACK #-} !Int }
  deriving (Generic, NFData)

consBQST :: a -> BQStrictTake a -> BQStrictTake a
consBQST a (BQStrictTake q l) = BQStrictTake (SL.take l $ SL.Cons a q) l

---

data BQStrictCount a = BQStrictCount
  { _bqsc      :: !(SL.List a)
  , _bqscLimit :: {-# UNPACK #-} !Int
  , _bqscSize  :: {-# UNPACK #-} !Int }
  deriving (Generic, NFData)

consBQSC :: a -> BQStrictCount a -> BQStrictCount a
consBQSC a (BQStrictCount q l s)
  | s == l = BQStrictCount (SL.Cons a $ SL.init q) l s
  | otherwise = BQStrictCount (SL.Cons a q) l (succ s)

---

data BQSeq a = BQSeq
  { _bqs      :: !(Seq a)
  , _bqsLimit :: {-# UNPACK #-} !Int
  , _bqsSize  :: {-# UNPACK #-} !Int }
  deriving (Generic, NFData)

instance Functor BQSeq where
  fmap f (BQSeq q l s) = BQSeq (f <$> q) l s

instance Foldable BQSeq where
  foldMap f (BQSeq q _ _) = foldMap f q

consSeq :: a -> BQSeq a -> BQSeq a
consSeq a (BQSeq Empty l _) = BQSeq (Seq.singleton a) l 1
consSeq a (BQSeq q@(rest :|> _) l s)
  | s == l = BQSeq (a <| rest) l s
  | otherwise = BQSeq (a <| q) l (succ s)

avgSeq :: BQSeq Int -> Int
avgSeq (BQSeq q _ s) = floor $ foldl' (+) 0 q % s

---

data BQVec a = BQVec
  { _bqv      :: !(VS.Vector a)
  , _bqvLimit :: {-# UNPACK #-} !Int
  , _cursor   :: {-# UNPACK #-} !Int }
  deriving (Generic, NFData)

singleton :: VSM.Storable a => a -> Int -> BQVec a
singleton a l = BQVec (VS.fromList . take l $ repeat a) l 1

consVec :: VSM.Storable a => a -> BQVec a -> BQVec a
consVec a (BQVec v l c) = BQVec v' l c'
  where
    c' = succ c `mod` l
    -- v' = VS.unsafeUpd v [(c, a)]
    v' = (VS.//) v [(c, a)]
    -- v' = runST $ do
    --   mv <- VS.unsafeThaw v
    --   VSM.unsafeModify mv (const a) c
    --   VS.unsafeFreeze mv

avgVec :: BQVec Int -> Int
avgVec (BQVec q _ s) = floor $ VS.foldl' (+) 0 q % s

---

deriving instance (NFData a) => NFData (SL.List a)
