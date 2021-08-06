module Main where

import Data.Apartment (Apartment(..), Bezirk (..))
import Data.Fixture.Apartments (apartments)

type Predicate = Apartment -> Bool

(&&&) :: Predicate -> Predicate -> Predicate
pa &&& pb = \apartment -> pa apartment && pb apartment

rentBelow :: Float -> Predicate
rentBelow maxRent Apartment { rent } = rent < maxRent

inBezirk :: Bezirk -> Predicate
inBezirk whichBezirk Apartment { bezirk } = bezirk == whichBezirk

areaAtLeast :: Float -> Predicate
areaAtLeast minArea Apartment { area } = area >= minArea

findApartment :: Predicate -> [Apartment]
findApartment p = filter p apartments

r1 :: [Bezirk]
r1 = map (\Apartment { bezirk } -> bezirk) (findApartment (rentBelow 700 &&& inBezirk Neukölln &&& areaAtLeast 50))

-- >>> (findApartment (rentBelow 700 &&& inBezirk Neukölln &&& areaAtLeast 50))
-- [Apartment {address = "Marienfelder Allee 27", rooms = 2, rent = 600.0, bezirk = Neukölln, area = 52.5, floor = Floor 4, lift = True}]

main :: IO ()
main = mempty
