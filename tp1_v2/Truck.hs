module Truck ( Truck, newT, freeCellsT, loadT, unloadT, netT )
  where

import Palet
import Stack
import Route
import Foreign (free)

data Truck = Tru [ Stack ] Route deriving (Eq, Show)

newT :: Int -> Int -> Route -> Truck  -- construye un camion según una cantidad de bahias, la altura de las mismas y una ruta
newT numBays height route = Tru (replicate numBays (newS height)) route

freeCellsT :: Truck -> Int            -- responde la celdas disponibles en el camion
freeCellsT (Tru listStack _) = sum(map freeCellsS listStack)

iterateStacks :: [Stack] -> Palet -> Route -> [Stack]
iterateStacks [] _ _ = []  
iterateStacks (s:ss) pal rou
    | (netP pal + netS s) > 10 = s : iterateStacks ss pal rou  
    | holdsS s pal rou = stackS s pal : ss  
    | otherwise = s : iterateStacks ss pal rou  

loadT :: Truck -> Palet -> Truck      -- carga un palet en el camion
loadT (Tru stackL rou) pal | not (inRouteR rou (destinationP pal)) = error "City not in route"  -- Verifica si la ciudad destino del palet está en la ruta
                           | newStacks == stackL = error "Sin espacio suficiente"
                           | otherwise = Tru (newStacks) rou
                        where 
                            newStacks = iterateStacks stackL pal rou 


unloadStacks :: [Stack] -> String -> [Stack]
unloadStacks listStacks dest = map (\x -> popS x dest) listStacks  -- aplica popS a cada stack de la lista

unloadT :: Truck -> String -> Truck   -- responde un camion al que se le han descargado los paletes que podían descargarse en la ciudad
unloadT (Tru stackL rou) dest = Tru (unloadStacks stackL dest) rou 

netT :: Truck -> Int                  -- responde el peso neto en toneladas de los paletes en el camion
netT (Tru listStack _) = sum (map netS listStack)


