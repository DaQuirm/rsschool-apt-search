module Main where

import Data.Apartment ( Apartment(..), Bezirk(..), Predicate, Getter )
import Data.Fixture.Apartments (apartments)
import Data.Filter (Filter(..), Operator(..), runFilter)
import Data.Foldable (Foldable(foldl'))
import Data.List (intersect, union)
import Data.Range (Range (SingletonRange, InfiniteRange))
import qualified Data.Range as Range
import Control.Monad (foldM)

findApartment :: Predicate -> [Apartment]
findApartment p = filter p apartments

is :: Getter a -> (a -> Bool) -> Predicate
is = flip (.)

myFilter :: Filter
myFilter =
    -- Or
      -- [
        And
          [ RentExpr (LessThan 700)
          , BezirkExpr (AnyOf [Reinickendorf, Kreuzberg ])
          , RoomsExpr (GreaterThan 1)
          , AreaExpr (GreaterThan 50)
          , BezirkExpr (AnyOf [Charlottenburg, Neukölln, PrenzlauerBerg])
          ]
      -- , RentExpr (LessThan 400)
      -- ]

getBezirkOpType :: Operator Bezirk -> FilterType
getBezirkOpType (Equals bezirk)      = topExpr { bezirkType = [bezirk] }
getBezirkOpType (LessThan bezirk)    = bottomExpr
getBezirkOpType (GreaterThan bezirk) = bottomExpr
getBezirkOpType (AnyOf bs)           = topExpr { bezirkType = bs }

getNumericOpRange :: Operator a -> [Range a]
getNumericOpRange (Equals n)      = [SingletonRange n]
getNumericOpRange (LessThan n)    = [Range.ubi n]
getNumericOpRange (GreaterThan n) = [Range.lbi n]
getNumericOpRange (AnyOf ns)      = SingletonRange <$> ns

data FilterType = FilterType
  { bezirkType :: [Bezirk]
  , rentType   :: [Range Float]
  , roomsType  :: [Range Int]
  , areaType   :: [Range Float]
  } deriving Show

bottomExpr = FilterType
  { bezirkType = []
  , rentType   = []
  , roomsType  = []
  , areaType   = []
  }

topExpr = FilterType
  { bezirkType = enumFrom (toEnum 0)
  , rentType   = [InfiniteRange]
  , roomsType  = [InfiniteRange]
  , areaType   = [InfiniteRange]
  }

unifyAnd :: FilterType -> FilterType -> Either String FilterType
unifyAnd fl@FilterType
            { bezirkType = bs
            , roomsType
            , rentType
            , areaType
            }
         fr@FilterType
            { bezirkType = bs'
            , rentType   = rentType'
            , roomsType  = roomsType'
            , areaType   = areaType'
            } =
    case bs `intersect` bs' of
      [] -> Left $ "Could not unify bezirk type\n\t" <> show bs <> "\nwith type\n\t" <> show bs' <> "\nin an 'and' expression"
      xs -> Right
        FilterType
          { bezirkType = xs
          , roomsType  = roomsType `Range.intersection` roomsType'
          , rentType   = rentType `Range.intersection` rentType'
          , areaType   = areaType `Range.intersection` areaType'
          }

unifyOr :: FilterType -> FilterType -> Either String FilterType
unifyOr FilterType
            { bezirkType = bs
            , roomsType
            , rentType
            , areaType
            }
         FilterType
            { bezirkType = bs'
            , roomsType  = roomsType'
            , rentType   = rentType'
            , areaType   = areaType'
            } =
    Right FilterType
      { bezirkType = bs `union` bs'
      , roomsType  = roomsType `Range.union` roomsType'
      , rentType   = rentType `Range.union` rentType'
      , areaType   = areaType `Range.union` areaType'
      }

typeCheck :: Filter -> Either String FilterType
typeCheck (RoomsExpr op)  = pure $ topExpr { roomsType = getNumericOpRange op }
typeCheck (RentExpr op)   = pure $ topExpr { rentType = getNumericOpRange op }
typeCheck (BezirkExpr op) = pure $ getBezirkOpType op
typeCheck (AreaExpr op)   = pure $ topExpr { areaType = getNumericOpRange op }
typeCheck (LiftExpr op)   = pure bottomExpr
typeCheck (And fs)        = foldM (\t f -> typeCheck f >>= unifyAnd t) topExpr fs
typeCheck (Or fs)         = foldM (\t f -> typeCheck f >>= unifyOr t) bottomExpr fs

-- >>> typeCheck myFilter
-- Left "Could not unify bezirk type [Kreuzberg,Reinickendorf]\n\twith type [Charlottenburg,Neuk\246lln,PrenzlauerBerg]in an 'and' expression"

-- >>> show myFilter
-- "or[and[rent < 700.0,area > 50.0,bezirk any of [Charlottenburg,Neuk\246lln,PrenzlauerBerg],rooms > 2],rent < 400.0]"

r2 = findApartment (runFilter myFilter)

{-
>>> import qualified Data.Text.Lazy as TL
>>> import Text.Pretty.Simple
>>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
>>> prettyPrint r2
[]
-}

main :: IO ()
main = mempty
