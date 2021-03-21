module MonoidProperties where

import Data.Monoid
import Test.QuickCheck

prop_MonoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
prop_MonoidLeftIdentity x = x <> mempty == x

prop_MonoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
prop_MonoidRightIdentity x = x == mempty <> x

prop_MonoidAssociativity :: (Monoid a, Eq a) => a -> a -> a -> Bool
prop_MonoidAssociativity a b c = ((a <> b) <> c) == (a <> (b <> c))
