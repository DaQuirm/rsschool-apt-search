module Data.Filter where

import Data.Apartment ( Apartment(..), Predicate, Bezirk )

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
  | And        Filter Filter
  | Or         Filter Filter

instance Show Filter where
  show (RoomsExpr op)  = "rooms "  ++ show op
  show (RentExpr op)   = "rent "   ++ show op
  show (BezirkExpr op) = "bezirk " ++ show op
  show (AreaExpr op)   = "area "   ++ show op
  show (LiftExpr op)   = "lift "   ++ show op
  show (And fl fr)     = "(" ++ show fl ++ ") and (" ++ show fr ++")"
  show (Or fl fr)      = "(" ++ show fl ++ ") or (" ++ show fr ++")"

-- >>> show (RentExpr (LessThan 700))
-- "rent < 700.0"

(&&&) :: Predicate -> Predicate -> Predicate
pa &&& pb = \apartment -> pa apartment && pb apartment

(|||) :: Predicate -> Predicate -> Predicate
pa ||| pb = \apartment -> pa apartment || pb apartment

runFilter :: Filter -> Predicate
runFilter (RoomsExpr op)  Apartment { rooms }  = runOperator op rooms
runFilter (RentExpr op)   Apartment { rent }   = runOperator op rent
runFilter (BezirkExpr op) Apartment { bezirk } = runOperator op bezirk
runFilter (AreaExpr op)   Apartment { area }   = runOperator op area
runFilter (LiftExpr op)   Apartment { lift }   = runOperator op lift
runFilter (And fl fr)     apartment            = (runFilter fl &&& runFilter fr) apartment
runFilter (Or fl fr)      apartment            = (runFilter fl ||| runFilter fr) apartment
