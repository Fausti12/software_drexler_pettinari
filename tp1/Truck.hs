module Truck ( Truck, newT, freeCellsT) --  loadT, unloadT, netT )
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
sumFreeCells listStack 0 = freeCellsS(head listStack)
sumFreeCells listStack n = freeCellsS(listStack !! n) + sumFreeCells listStack (n-1)

freeCellsT (Tru listStack _) = sumFreeCells listStack (length listStack-1)

updateStack :: [Stack] -> Palet -> Int -> Stack 
updateStack listStack palet index = stackS (listStack !! index) palet

loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
-- hay que llamar holdsS por c/stack
modifyStack (Tru listStack route) palet | null stackHolds = error "Truck can´t load this Palet"
                              | otherwise = Tru (updateStack listStack palet (head stackHolds)) route
    where
      stackHolds = [y | y <- [0..length(listStack)-1], holdsS (listStack !! y) palet route == True]
    

--unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
--netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion


