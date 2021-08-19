module Main where

import Data.Apartment ( Apartment(..), Bezirk(..), Predicate, Getter )
import Data.Fixture.Apartments (apartments)
import Data.Filter (Filter(..), Operator(..), runFilter)

(&&&) :: Predicate -> Predicate -> Predicate
pa &&& pb = \apartment -> pa apartment && pb apartment

(|||) :: Predicate -> Predicate -> Predicate
pa ||| pb = \apartment -> pa apartment || pb apartment

anyOf :: Eq a => Getter a -> [a] -> Predicate
anyOf getter values = is getter (`elem` values)

findApartment :: Predicate -> [Apartment]
findApartment p = filter p apartments

is :: Getter a -> (a -> Bool) -> Predicate
is = flip (.)

r1 :: [Apartment]
r1 = findApartment
  (   rent `is` (< 700)
  &&& (area `is` (>= 50))
  &&& (bezirk `anyOf` [Charlottenburg, Neukölln, PrenzlauerBerg])
  &&& (rooms `is` (>= 2))
  )

myFilter :: Filter
myFilter = BezirkExpr (AnyOf [Charlottenburg, Neukölln, PrenzlauerBerg])

-- >>> show myFilter
-- "bezirk any of [Charlottenburg,Neuk\246lln,PrenzlauerBerg]"

r2 = findApartment (runFilter myFilter)

{-
>>> import qualified Data.Text.Lazy as TL
>>> import Text.Pretty.Simple
>>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
>>> prettyPrint r2
[ Apartment
    { address = "Otto-Suhr-Allee 114"
    , rooms = 1
    , rent = 620.0
    , bezirk = Charlottenburg
    , area = 50.0
    , floor = Ground
    , lift = False
    }
, Apartment
    { address = "Wilmersdorfer Straße 37"
    , rooms = 3
    , rent = 1400.0
    , bezirk = Charlottenburg
    , area = 89.0
    , floor = Floor 4
    , lift = False
    }
, Apartment
    { address = "Nogatstraße 31"
    , rooms = 1
    , rent = 516.0
    , bezirk = Neukölln
    , area = 43.0
    , floor = Floor 3
    , lift = False
    }
, Apartment
    { address = "Tempelhofer Feld"
    , rooms = 2
    , rent = 780.0
    , bezirk = Neukölln
    , area = 71.0
    , floor = Ground
    , lift = False
    }
, Apartment
    { address = "Marienfelder Allee 27"
    , rooms = 2
    , rent = 600.0
    , bezirk = Neukölln
    , area = 52.5
    , floor = Floor 4
    , lift = True
    }
, Apartment
    { address = "Dietrich-Bonhoeffer-Straße 2"
    , rooms = 2
    , rent = 702.0
    , bezirk = PrenzlauerBerg
    , area = 54.0
    , floor = Floor 1
    , lift = True
    }
, Apartment
    { address = "Dahlmannstrasse 2"
    , rooms = 2
    , rent = 850.0
    , bezirk = Charlottenburg
    , area = 72.0
    , floor = Floor 3
    , lift = True
    }
, Apartment
    { address = "Alt Moabit 37"
    , rooms = 2
    , rent = 389.62
    , bezirk = Charlottenburg
    , area = 59.85
    , floor = Floor 3
    , lift = False
    }
, Apartment
    { address = "Rauschener Alle 1"
    , rooms = 2
    , rent = 800.0
    , bezirk = Charlottenburg
    , area = 58.0
    , floor = Floor 3
    , lift = False
    }
, Apartment
    { address = "Kurfürstendamm 74"
    , rooms = 3
    , rent = 1055.0
    , bezirk = Charlottenburg
    , area = 84.41
    , floor = Floor 2
    , lift = False
    }
, Apartment
    { address = "Kurfürstendamm 105"
    , rooms = 1
    , rent = 495.0
    , bezirk = Charlottenburg
    , area = 41.0
    , floor = Floor 3
    , lift = True
    }
, Apartment
    { address = "Müllerstr. 29"
    , rooms = 2
    , rent = 628.0
    , bezirk = PrenzlauerBerg
    , area = 56.13
    , floor = Ground
    , lift = False
    }
]
-}

main :: IO ()
main = mempty
