module Data.Filter where

import Data.Apartment ( Apartment(Apartment, rent), Predicate )

data Property
  = Rent

data Operator
  = LessThan
  | GreaterThan

type Value = Float

data Filter
  = Filter Property Operator Value

-- >>> :t Filter
-- Filter :: Property -> Operator -> Value -> Filter

runFilter :: Filter -> Predicate
runFilter (Filter Rent LessThan value)    Apartment { rent } = rent < value
runFilter (Filter Rent GreaterThan value) Apartment { rent } = rent > value

toString :: Filter -> String
toString (Filter Rent LessThan value)    = "< " ++ show value
toString (Filter Rent GreaterThan value) = "> " ++ show value
