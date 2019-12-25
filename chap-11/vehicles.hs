module Vehicles where

  data Price 
    = Price Integer deriving (Eq, Show)
  
  data Manufacturer 
    = Mini
    | Mazda 
    | Tata deriving (Eq, Show)
  

  data Airplane 
    = PapuAir
    | Catapults
    | TakeYourChancesUnited deriving (Eq, Show)

  data Size 
    = Small
    | Medium
    | Large
    | Huge deriving (Eq, Show)

  data Vehicle
    = Car Manufacturer Price
    | Plane Airplane Size deriving (Eq, Show)

  myCar = Car Mini (Price 14000)

  urCar = Car Mazda (Price 20000)

  clownCar = Car Tata (Price 7000)

  doge = Plane PapuAir Huge

  pia = Plane TakeYourChancesUnited Small

  isCar :: Vehicle -> Bool
  -- isCar veh = 
  --   case veh of 
  --     Car _ _ -> True
  --     _ -> False
  isCar (Car _ _) = True
  isCar _ = False
  
  isPlane :: Vehicle -> Bool
  -- isPlane = not . isCar  -- coz we cant be sure if Plane and Car are the only data constructors of Vehicle
  isPlane (Plane _ _ ) = True
  isPlane _ = False

  areCars :: [Vehicle] -> [Bool]
  areCars = map isCar 

  getManu :: Vehicle -> Manufacturer
  getManu (Car man _) = man 
  -- crahsed if a plane is input instead of a Car
  -- because not all possible data constructors are being accounted for