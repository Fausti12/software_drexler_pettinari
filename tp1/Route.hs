module Route ( Route, newR, inOrderR, inRouteR )
  where

data Route = Rou [ String ] deriving (Eq, Show)

newR :: [ String ] -> Route                    -- construye una ruta segun una lista de ciudades
newR cities | null cities = error "Empty route"  
            | otherwise = Rou cities

inOrderR :: Route -> String -> String -> Bool  -- indica si la primer ciudad consultada esta antes que la segunda ciudad en la ruta
inOrderR (Rou cities) city1 city2
    | inRouteCity1 == False || inRouteCity2 == False = error "One or both cities not found in route"
    | pos1 < pos2 = True
    | otherwise = False
  where
    pos1 = positionCity cities city1
    pos2 = positionCity cities city2 
    inRouteCity1 = inRouteR (Rou cities) city1
    inRouteCity2 = inRouteR (Rou cities) city2

positionCity :: [String] -> String -> Int
positionCity cities city = foldr (\index acc -> if cities !! index == city then index else acc) (-1) [0..length cities - 1]

inRouteR :: Route -> String -> Bool -- indica si la ciudad consultada está en la ruta
inRouteR (Rou cities) city = elem city cities