module Route ( Route, newR, inOrderR )
  where

data Route = Rou [ String ] deriving (Eq, Show)

newR :: [ String ] -> Route                    -- construye una ruta segun una lista de ciudades
newR cities = Rou cities

inOrderR :: Route -> String -> String -> Bool  -- indica si la primer ciudad consultada esta antes que la segunda ciudad en la ruta
positionCity :: [String] -> String -> Int
positionCity cities city
    | null index = -1
    | otherwise = head index
  where
    index = [y | y <- [0..length(cities)-1], cities !! y == city]
-- Buscar forma que funcione si NO HAY COINCIDENCIAS

inOrderR (Rou cities) city1 city2
    | pos1 == -1 || pos2 == -1 = error "One or both cities not found in route"
    | pos1 < pos2 = True
    | otherwise = False
  where
    pos1 = positionCity cities city1
    pos2 = positionCity cities city2 



inRouteR :: Route -> String -> Bool -- indica si la ciudad consultada está en la ruta
inRouteR (Rou cities) city = elem city cities