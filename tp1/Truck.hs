module Truck ( Truck, newT, freeCellsT)--, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route

data Truck = Tru [ Stack ] Route deriving (Eq, Show)

newT :: Int -> Int -> Route -> Truck  -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
createStackList :: Int -> Int -> [Stack]
--createStackList 1 height listStack = listStack ++ [newS height]
--createStackList numBays height listStack = createStackList((numBays - 1) height listStack ++ [newS height] )
createStackList 1 height = [newS height]
createStackList numBays height  = [newS height] ++ createStackList (numBays - 1) height 
newT numBays height route = Tru (createStackList numBays height) route

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
sumCells :: [Stack] -> Int -> Int
sumCells stackL 0 = freeCellsS(stackL !! 0)
sumCells stackL n = freeCellsS(stackL !! n) + sumCells stackL (n-1)
freeCellsT (Tru stackL _) = sumCells stackL (length(stackL)-1)


--loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
--unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
--netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion


