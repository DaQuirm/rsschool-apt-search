module Main where

import Data.Apartment (Apartment(..), Bezirk (..))
import Data.Fixture.Apartments (apartments)

type Predicate = Apartment -> Bool
type Getter a = Apartment -> a

(&&&) :: Predicate -> Predicate -> Predicate
pa &&& pb = \apartment -> pa apartment && pb apartment

(|||) :: Predicate -> Predicate -> Predicate
pa ||| pb = \apartment -> pa apartment || pb apartment

compareWith :: (a -> b -> Bool) -> Getter a -> b -> Predicate
compareWith op getter value apartment = getter apartment `op` value

below :: Ord a => Getter a -> a -> Predicate
below = compareWith (<)

atLeast :: Ord a => Getter a -> a -> Predicate
atLeast = compareWith (>=)

is :: Eq a => Getter a -> a -> Predicate
is = compareWith (==)

anyOf :: Eq a => Getter a -> [a] -> Predicate
anyOf = compareWith elem

findApartment :: Predicate -> [Apartment]
findApartment p = filter p apartments

r1 :: [Apartment]
r1 = findApartment
  (   rent `below` 700
  &&& (area `atLeast` 50)
  &&& (bezirk `anyOf` [Charlottenburg, Neukölln, PrenzlauerBerg])
  &&& (rooms `atLeast` 2)
  )

-- >>> r1
-- [Apartment {address = "Marienfelder Allee 27", rooms = 2, rent = 600.0, bezirk = Neukölln, area = 52.5, floor = Floor 4, lift = True},Apartment {address = "Alt Moabit 37", rooms = 2, rent = 389.62, bezirk = Charlottenburg, area = 59.85, floor = Floor 3, lift = False},Apartment {address = "M\252llerstr. 29", rooms = 2, rent = 628.0, bezirk = PrenzlauerBerg, area = 56.13, floor = Ground, lift = False}]

main :: IO ()
main = mempty
