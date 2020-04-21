module AssocListSol where

{-T12.1.1-}
import Prelude hiding (lookup)
import Data.List (nub)
import Test.QuickCheck

newtype Map k v = undefined
  deriving (Show)

empty :: Map k v
empty = undefined

insert :: Eq k => k -> v -> Map k v -> Map k v
insert = undefined

lookup :: Eq k => k -> Map k v -> Maybe v
lookup = undefined

delete :: Eq k => k -> Map k v -> Map k v
delete = undefined

keys :: Map k v -> [k]
keys = undefined

{-T12.1.2-}
invar :: Eq k => Map k v -> Bool
invar = undefined

-- generates an aribtrary Map for your QuickCheck tests
instance (Eq k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = do k <- arbitrary
                 v <- arbitrary
                 return $ Map $ zip (nub k) v
