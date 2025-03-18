module Truck ( Truck, newT, freeCellsT, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route
import Foreign (free)

data Truck = Tru [ Stack ] Route deriving (Eq, Show)

createStackList :: Int -> Int -> [Stack]
createStackList 1 height = [newS height]
createStackList numBays height  = [newS height] ++ createStackList (numBays - 1) height 

newT :: Int -> Int -> Route -> Truck  -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
newT numBays height route | numBays < 1 = error "La cantidad de bahías debe ser mayor a 0"
                          | height < 1 = error "La altura de las bahías debe ser mayor a 0"
                          | otherwise = Tru (createStackList numBays height) route

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
sumFreeCells:: [Stack] -> Int -> Int
sumFreeCells [] _ = 0 -- ver si es necesario
sumFreeCells listStack 0 = freeCellsS(head listStack)
sumFreeCells listStack n = freeCellsS(listStack !! n) + sumFreeCells listStack (n-1)

freeCellsT (Tru listStack _) = sumFreeCells listStack (length listStack-1)


-- VER SI DEVOLVER ERROR O = TRUCK CON EL PALET NO CARGADO
iterateStacks :: [Stack] -> Palet -> Route -> [Stack]
iterateStacks [] _ _ = []  -- si no hay stacks, devuelvo lista vacía
iterateStacks (s:ss) pal rou
    | (netP pal + netS s) > 10 = s : iterateStacks ss pal rou  -- si bahía pesara más de 10 toneladas, no lo cargo
    | holdsS s pal rou = stackS s pal : ss  -- si holdsS es True, apilo en stack y devuelvo lista con ese stack y el resto
    | otherwise = s : iterateStacks ss pal rou  -- si holdsS es False, devuelvo lista con el stack y sigo iterando

loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
loadT (Tru stackL rou) pal | not (inRouteR rou (destinationP pal)) = error "City not in route"  -- Verifica si la ciudad destino del palet está en la ruta
                           | newStacks == stackL = error "Sin espacio suficiente"
                           | otherwise = Tru (iterateStacks stackL pal rou) rou
                        where 
                            newStacks = iterateStacks stackL pal rou 


unloadStacks :: [Stack] -> String -> [Stack]
unloadStacks [] _ = []
unloadStacks (s:ss) dest = popS s dest : unloadStacks ss dest


-- findIndexDestiny :: Route -> String -> [Int]
-- findIndexDestiny route dest =  [y | y <- [0..length(route)-1], route !! y == dest]


unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
--unloadT (Tru stackL rou) dest = Tru (unloadStacks stackL dest) newR (drop ((findIndexDestiny rou dest) !! 0 + 1) cities)
  --  where
    --(Rou cities) = rou
unloadT (Tru stackL rou) dest = Tru (unloadStacks stackL dest) rou 
-- se podría sacar el 1er elemento de route (quedarnos solo con lo que va después de la ruta que nos pasan)


sumWeightsBays :: [Stack] -> Int
sumWeightsBays [] = 0
sumWeightsBays (p:ps) = netS p + sumWeightsBays ps


netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru listStack _) = sumWeightsBays listStack


