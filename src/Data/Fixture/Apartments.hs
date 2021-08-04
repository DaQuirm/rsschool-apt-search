module Data.Fixture.Apartments where

import Prelude hiding (floor)
import Data.Apartment (Apartment(..), Bezirk (..), Floor (..))

apartments :: [Apartment]
apartments =
  [ Apartment
    { address = "Otto-Suhr-Allee 114"
    , rooms   = 1
    , rent    = 620.0
    , bezirk  = Charlottenburg
    , area    = 50.0
    , floor   = Ground
    , lift    = False
    }
  , Apartment
    { address = "Wilmersdorfer Straße 37"
    , rooms   = 3
    , rent    = 1400.0
    , bezirk  = Charlottenburg
    , area    = 89.0
    , floor   = Floor 4
    , lift    = False
    }
  , Apartment
    { address = "Nogatstraße 31"
    , rooms   = 1
    , rent    = 516.0
    , bezirk  = Neukölln
    , area    = 43.0
    , floor   = Floor 3
    , lift    = False
    }
  , Apartment
    { address = "Beethovenstr. 16"
    , rooms   = 3
    , rent    = 1093.0
    , bezirk  = Schöneberg
    , area    = 99.83
    , floor   = Floor 3
    , lift    = False
    }
  , Apartment
    { address = "Mehringdamm 43"
    , rooms   = 3
    , rent    = 740.0
    , bezirk  = Kreuzberg
    , area    = 85.0
    , floor   = Floor 3
    , lift    = True
    }
  , Apartment
    { address = "Tempelhofer Feld"
    , rooms   = 2
    , rent    = 780.0
    , bezirk  = Neukölln
    , area    = 71.0
    , floor   = Ground
    , lift    = False
    }
  , Apartment
    { address = "Marienfelder Allee 27"
    , rooms   = 2
    , rent    = 600.0
    , bezirk  = Neukölln
    , area    = 52.5
    , floor   = Floor 4
    , lift    = True
    }
  , Apartment
    { address = "Dietrich-Bonhoeffer-Straße 2"
    , rooms   = 2
    , rent    = 702.0
    , bezirk  = PrenzlauerBerg
    , area    = 54
    , floor   = Floor 1
    , lift    = True
    }
  , Apartment
    { address = "Kurfürstenstr. 15"
    , rooms   = 2
    , rent    = 1300.0
    , bezirk  = Mitte
    , area    = 110
    , floor   = Floor 4
    , lift    = True
    }
  , Apartment
    { address = "Dahlmannstrasse 2"
    , rooms   = 2
    , rent    = 850.0
    , bezirk  = Charlottenburg
    , area    = 72
    , floor   = Floor 3
    , lift    = True
    }
  , Apartment
    { address = "Alt Moabit 37"
    , rooms   = 2
    , rent    = 389.62
    , bezirk  = Charlottenburg
    , area    = 59.85
    , floor   = Floor 3
    , lift    = False
    }
  , Apartment
    { address = "Rauschener Alle 1"
    , rooms   = 2
    , rent    = 800.0
    , bezirk  = Charlottenburg
    , area    = 58.0
    , floor   = Floor 3
    , lift    = False
    }
  , Apartment
    { address = "Kurfürstendamm 74"
    , rooms   = 3
    , rent    = 1055.0
    , bezirk  = Charlottenburg
    , area    = 84.41
    , floor   = Floor 2
    , lift    = False
    }
  , Apartment
    { address = "Freiheitsweg 13"
    , rooms   = 1
    , rent    = 313.48
    , bezirk  = Reinickendorf
    , area    = 37.21
    , floor   = Attic
    , lift    = False
    }
  , Apartment
    { address = "Steglitzer Damm 51B"
    , rooms   = 2
    , rent    = 768.5
    , bezirk  = Reinickendorf
    , area    = 58.0
    , floor   = Floor 1
    , lift    = True
    }
  , Apartment
    { address = "Kurfürstendamm 105"
    , rooms   = 1
    , rent    = 495.0
    , bezirk  = Charlottenburg
    , area    = 41.0
    , floor   = Floor 3
    , lift    = True
    }
  , Apartment
    { address = "Breite Straße 29"
    , rooms   = 4
    , rent    = 820.0
    , bezirk  = Spandau
    , area    = 84.0
    , floor   = Floor 2
    , lift    = True
    }
  , Apartment
    { address = "Müllerstr. 29"
    , rooms   = 2
    , rent    = 628.0
    , bezirk  = PrenzlauerBerg
    , area    = 56.13
    , floor   = Ground
    , lift    = False
    }

  ]
