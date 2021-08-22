module Data.Apartment where

data Bezirk
  = Mitte
  | Friedrichshain
  | Kreuzberg
  | PrenzlauerBerg
  | Charlottenburg
  | Spandau
  | Schöneberg
  | Neukölln
  | MarzahnHellersdorf
  | Lichtenberg
  | Reinickendorf
  deriving (Eq, Ord, Show)

data Floor
  = Ground
  | Floor Int
  | Attic
  deriving (Eq, Show)

data Apartment = Apartment
  { address :: String
  , rooms   :: Int
  , rent    :: Float
  , bezirk  :: Bezirk
  , area    :: Float
  , floor   :: Floor
  , lift    :: Bool
  } deriving (Eq, Show)

type Predicate = Apartment -> Bool
type Getter a = Apartment -> a
