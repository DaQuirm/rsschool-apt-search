module Data.Filter where

import Data.Apartment ( Apartment(..), Predicate, Bezirk )

data Operator a
  = LessThan a
  | GreaterThan a
  | AnyOf [a]
  | Equal a

instance Show a => Show (Operator a) where
  show (LessThan n)    = "< " ++ show n
  show (GreaterThan n) = "> " ++ show n
  show (AnyOf xs)      = "any of " ++ show xs
  show (Equal n)       = "== " ++ show n

runOperator :: Ord a => Operator a -> a -> Bool
runOperator (LessThan n)    = (< n)
runOperator (GreaterThan n) = (> n)
runOperator (AnyOf xs)      = (`elem` xs)
runOperator (Equal n)       = (= n)

data Filter
  = RoomsExpr  (Operator Int)
  | RentExpr   (Operator Float)
  | BezirkExpr (Operator Bezirk)
  | AreaExpr   (Operator Float)
  | LiftExpr   (Operator Bool)

instance Show Filter where
  show (RoomsExpr op)  = "rooms "  ++ show op
  show (RentExpr op)   = "rent "   ++ show op
  show (BezirkExpr op) = "bezirk " ++ show op
  show (AreaExpr op)   = "area "   ++ show op
  show (LiftExpr op)   = "lift "   ++ show op

-- >>> show (RentExpr (LessThan 700))
-- "rent < 700.0"

runFilter :: Filter -> Predicate
runFilter (RoomsExpr op)  Apartment { rooms }  = runOperator op rooms
runFilter (RentExpr op)   Apartment { rent }   = runOperator op rent
runFilter (BezirkExpr op) Apartment { bezirk } = runOperator op bezirk
runFilter (AreaExpr op)   Apartment { area }   = runOperator op area
runFilter (LiftExpr op)   Apartment { lift }   = runOperator op lift
