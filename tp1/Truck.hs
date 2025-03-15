module Truck ( Truck, newT, freeCellsT, loadT, unloadT) --netT )
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
newT numBays height route = Tru (createStackList numBays height) route

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
sumFreeCells:: [Stack] -> Int -> Int
sumFreeCells [] _ = 0 -- ver si es necesario
sumFreeCells listStack 0 = freeCellsS(head listStack)
sumFreeCells listStack n = freeCellsS(listStack !! n) + sumFreeCells listStack (n-1)

freeCellsT (Tru listStack _) = sumFreeCells listStack (length listStack-1)

-- updateStack :: [Stack] -> Palet -> Int -> Stack 
-- updateStack listStack palet index = stackS (listStack !! index) palet

--loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
-- hay que llamar holdsS por c/stack
--modifyStack (Tru listStack route) palet | null stackHolds = error "Truck can´t load this Palet"
                              -- | otherwise = Tru (updateStack listStack palet (head stackHolds)) route
 --   where
   --   stackHolds = [y | y <- [0..length(listStack)-1], holdsS (listStack !! y) palet route == True]
    

iterateStacks :: [Stack] -> Palet -> Route -> [Stack]
iterateStacks [] _ _ = []  -- si no hay stacks, devuelvo lista vacía
iterateStacks (s:ss) pal rou
    | (netP pal + netS s) > 10 = s : iterateStacks ss pal rou  -- si el palet pesa más de 10 toneladas, no lo cargo
    | holdsS s pal rou = stackS s pal : ss  -- si holdsS es True, apilo en stack y devuelvo lista con ese stack y el resto
    | otherwise = s : iterateStacks ss pal rou  -- si holdsS es False, devuelvo lista con el stack y sigo iterando

loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
loadT (Tru stackL rou) pal | not (inRouteR rou (destinationP pal)) = error "City not in route"  -- Verifica si la ciudad destino del palet está en la ruta
                           | otherwise = Tru (iterateStacks stackL pal rou) rou


unloadStacks :: [Stack] -> String -> [Stack]
unloadStacks [] _ = []
unloadStacks stackL dest | null stackL = []  -- al crear Truck siempre hay al menos un stack -> no hay que chequear si null
                         | otherwise = popS (head stackL) dest : unloadStacks (tail stackL) dest


unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
unloadT (Tru stackL rou) dest = Tru (unloadStacks stackL dest) rou


sumWeights :: [Stack] -> Int
sumWeights [] = 0
sumWeights (p:ps) = netS p + sumWeights ps


netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru listStack _) = sumWeights listStack


