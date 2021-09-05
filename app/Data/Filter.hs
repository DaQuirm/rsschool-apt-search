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
runOperator (LessThan n)    = (<= n)
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

runFilter :: Filter -> Predicate
runFilter (RoomsExpr op)  = runOperator op . rooms
runFilter (RentExpr op)   = runOperator op . rent
runFilter (BezirkExpr op) = runOperator op . bezirk
runFilter (AreaExpr op)   = runOperator op . area
runFilter (LiftExpr op)   = runOperator op . lift
runFilter (And fs)        = getAll . foldMap ((All .) . runFilter) fs
runFilter (Or fs)         = getAny . foldMap ((Any .) . runFilter) fs
