module Main where

import Data.Apartment (Apartment(..), Bezirk (Charlottenburg))
import Data.Fixture.Apartments (apartments)

r0 :: [Apartment]
r0 = filter (\Apartment { rent, bezirk } -> rent < 800 && bezirk == Charlottenburg) apartments

r1 :: [Bezirk]
r1 = map (\Apartment { bezirk } -> bezirk) r0

-- >>> r0
-- [Apartment {address = "Otto-Suhr-Allee 114", rooms = 1, rent = 620.0, bezirk = Charlottenburg, area = 50.0, floor = Ground, lift = False},Apartment {address = "Alt Moabit 37", rooms = 2, rent = 389.62, bezirk = Charlottenburg, area = 59.85, floor = Floor 3, lift = False},Apartment {address = "Kurf\252rstendamm 105", rooms = 1, rent = 495.0, bezirk = Charlottenburg, area = 41.0, floor = Floor 3, lift = True}]

main :: IO ()
main = mempty
