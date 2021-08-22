module Main where

import Data.Apartment ( Apartment(..), Bezirk(..), Predicate, Getter )
import Data.Fixture.Apartments (apartments)
import Data.Filter (Filter(..), Operator(..), runFilter)

findApartment :: Predicate -> [Apartment]
findApartment p = filter p apartments

is :: Getter a -> (a -> Bool) -> Predicate
is = flip (.)

myFilter :: Filter
myFilter =
    Or
      (And
        (RentExpr (LessThan 700))
        (And
          (AreaExpr (GreaterThan 50))
          (And
            (BezirkExpr (AnyOf [Charlottenburg, Neukölln, PrenzlauerBerg]))
            (RoomsExpr (GreaterThan 2)))))
      (RentExpr (LessThan 400))

-- >>> show myFilter
-- "(rent < 700.0) and (area > 50.0)"

r2 = findApartment (runFilter myFilter)

{-
>>> import qualified Data.Text.Lazy as TL
>>> import Text.Pretty.Simple
>>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
>>> prettyPrint r2
[ Apartment
    { address = "Marienfelder Allee 27"
    , rooms = 2
    , rent = 600.0
    , bezirk = Neukölln
    , area = 52.5
    , floor = Floor 4
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
    { address = "Freiheitsweg 13"
    , rooms = 1
    , rent = 313.48
    , bezirk = Reinickendorf
    , area = 37.21
    , floor = Attic
    , lift = False
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
