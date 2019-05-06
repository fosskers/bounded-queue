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
import qualified Deque.Strict as DQ
import           GHC.Exts (fromList)
import           GHC.Generics (Generic)
import qualified StrictList as SL

---

-- | A bounded, strict `DQ.Deque` which drops old elements off its right side as
-- new elements are added (`DQ.cons`'d) to the left.
--
-- Not terribly robust.
--
data BDQ a = BDQ { _bdq :: !(DQ.Deque a), _bdqLimit :: !Word }

-- | Create a `BDQ` that contains @n@ copies of some `a`.
--
newBDQ :: a -> Word -> BDQ a
newBDQ a n = BDQ as n
  where
    as = fromList . take (fromIntegral n) $ repeat a

-- | \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
consBDQ :: a -> BDQ a -> BDQ a
consBDQ a (BDQ q l) = BDQ (DQ.cons a $ DQ.init q) l

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

deriving instance (NFData a) => NFData (SL.List a)
