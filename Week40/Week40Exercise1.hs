module Week40Exercise1 where

data PlanetarySystem = PlanetarySystem
  { sunName :: String,
    planets :: [Planet]
  }
  deriving (Eq, Show)

data Planet = Planet
  { planetName :: String,
    moons :: [Moon]
  }
  deriving (Eq, Show)

newtype Moon = Moon {moonName :: String}
  deriving (Eq, Show)

countMoonsForPlanet :: Planet -> Integer
countMoonsForPlanet p = fromIntegral $ length (moons p)

numberOfMoons :: PlanetarySystem -> Integer
numberOfMoons sys = 
  sum [countMoonsForPlanet planet | planet <- planets sys]

atLeastOneMoon :: PlanetarySystem -> [Planet]
atLeastOneMoon (PlanetarySystem _ planets) = atLeastOneMoon' planets
  where
    atLeastOneMoon' :: [Planet] -> [Planet]
    atLeastOneMoon' [] = []
    atLeastOneMoon' (x:xs) = 
      if countMoonsForPlanet x > 0
        then x : atLeastOneMoon' xs
      else atLeastOneMoon' xs

solarSystem :: PlanetarySystem
solarSystem =
  PlanetarySystem
    { sunName = "The Sun",
      planets =
        [ Planet
            { planetName = "Mercury",
              moons = []
            },
          Planet
            { planetName = "Venus",
              moons = []
            },
          Planet
            { planetName = "Earth",
              moons =
                [ Moon {moonName = "The Moon"}
                ]
            },
          Planet
            { planetName = "Mars",
              moons =
                [ Moon {moonName = "Deimos"},
                  Moon {moonName = "Phobos"}
                ]
            },
          Planet
            { planetName = "Jupiter",
              moons =
                [ Moon {moonName = "Io"},
                  Moon {moonName = "Europa"},
                  Moon {moonName = "Ganymede"},
                  Moon {moonName = "Callisto"}
                ]
            },
          Planet
            { planetName = "Saturn",
              moons =
                [ Moon {moonName = "Mimas"},
                  Moon {moonName = "Enceladus"},
                  Moon {moonName = "Tethys"},
                  Moon {moonName = "Dione"},
                  Moon {moonName = "Rhea"},
                  Moon {moonName = "Titan"},
                  Moon {moonName = "Hyperion"},
                  Moon {moonName = "Iapetus"},
                  Moon {moonName = "Phoebe"}
                ]
            },
          Planet
            { planetName = "Uranus",
              moons =
                [ Moon {moonName = "Miranda"},
                  Moon {moonName = "Ariel"},
                  Moon {moonName = "Umbriel"},
                  Moon {moonName = "Titania"},
                  Moon {moonName = "Oberon"}
                ]
            },
          Planet
            { planetName = "Neptune",
              moons =
                [ Moon {moonName = "Proteus"},
                  Moon {moonName = "Triton"},
                  Moon {moonName = "Nereid"}
                ]
            }
        ]
    }