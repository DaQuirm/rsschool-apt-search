module Data.Filter where

import Data.Apartment ( Apartment(..), Predicate, Bezirk )
import Data.Monoid (Any(..), All (..))

data Operator a
  = Equals a
  | LessThan a
  | GreaterThan a
  | AnyOf [a]

instance Show a => Show (Operator a) where
  show (Equals n)      = "= " ++ show n
  show (LessThan n)    = "< " ++ show n
  show (GreaterThan n) = "> " ++ show n
  show (AnyOf xs)      = "any of " ++ show xs

runOperator :: Ord a => Operator a -> a -> Bool
runOperator (Equals n)      = (== n)
runOperator (LessThan n)    = (< n)
runOperator (GreaterThan n) = (>= n)
runOperator (AnyOf xs)      = (`elem` xs)

data Filter
  = RoomsExpr  (Operator Int)
  | RentExpr   (Operator Float)
  | BezirkExpr (Operator Bezirk)
  | AreaExpr   (Operator Float)
  | LiftExpr   (Operator Bool)
  | And        [Filter]
  | Or         [Filter]

instance Show Filter where
  show (RoomsExpr op)  = "rooms "  ++ show op
  show (RentExpr op)   = "rent "   ++ show op
  show (BezirkExpr op) = "bezirk " ++ show op
  show (AreaExpr op)   = "area "   ++ show op
  show (LiftExpr op)   = "lift "   ++ show op
  show (And fs)        = "and" ++ show fs
  show (Or fs)         = "or" ++ show fs

-- >>> show (RentExpr (LessThan 700))
-- "rent < 700.0"

(&&&) :: Predicate -> Predicate -> Predicate
pa &&& pb = getAll . (All . pa <> All . pb)

(|||) :: Predicate -> Predicate -> Predicate
pa ||| pb = getAny . (Any . pa <> Any . pb)

runFilter :: Filter -> Predicate
runFilter (RoomsExpr op)  = runOperator op . rooms
runFilter (RentExpr op)   = runOperator op . rent
runFilter (BezirkExpr op) = runOperator op . bezirk
runFilter (AreaExpr op)   = runOperator op . area
runFilter (LiftExpr op)   = runOperator op . lift
runFilter (And fs)        = andP (map runFilter fs)
runFilter (Or fs)         = orP (map runFilter fs)

-- List = 1 &&& (2 &&& (3 &&& 0)) --> sum = foldl (+) 0

-- >>> :t and
-- and :: [Bool] -> Bool

-- >>> :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b

andP :: [Predicate] -> Predicate
andP = foldr (&&&) (const True)

orP :: [Predicate] -> Predicate
orP = foldr (|||) (const False)

-- newtype Sum = Sum { getSum :: Int } deriving Show
-- newtype Product = Product { getProduct :: Int } deriving Show

-- instance Semigroup Sum where
--   (Sum x) <> (Sum y) = Sum (x + y)

-- instance Semigroup Product where
--   (Product x) <> (Product y) = Product (x * y)

-- >>> getSum ((Sum 3) <> (Sum 4))
-- 7

-- >>> getProduct ((Product 3) <> (Product 4))
-- 12

-- >>> :t (&&)
-- (&&) :: Bool -> Bool -> Bool

-- >>> :t (||)
-- (||) :: Bool -> Bool -> Bool

-- instance Monoid Sum where
--   mempty = Sum 0

-- instance Monoid Product where
--   mempty = Product 1

-- >>> mconcat [Sum 3, Sum 4, Sum 5]
-- Sum {getSum = 12}

-- Apartment -> Bool

-- Apartment -> All
-- Apartment -> Any

-- >>> getAll ((mconcat [const (All False), const (All True), const (All True), const (All False)]) 37)
-- False

-- >>> :t All
-- All :: Bool -> All
