module Stack ( Stack, newS, freeCellsS, stackS, netS, holdsS, popS )
  where

import Palet
import Route

data Stack = Sta [ Palet ] Int deriving (Eq, Show)

newS :: Int -> Stack                      -- construye una Pila con la capacidad indicada  -> capacidad = cant Palets creo
newS capacity = Sta [] capacity 

freeCellsS :: Stack -> Int                -- responde con las celdas disponibles en la pila
freeCellsS (Sta listPalets capacity) =  capacity - length(listPalets)

stackS :: Stack -> Palet -> Stack         -- apila el palet indicado en la pila
stackS (Sta listPalets capacity ) palet =  Sta (listPalets ++ [palet]) capacity

--sumWeights :: [Palet] -> Int
--sumWeights listPalets = sum (map netP listPalets)
--sumWeights [] = 0
--sumWeights (p:ps) = netP p + sumWeights ps

netS :: Stack -> Int                      -- responde el peso neto de los paletes en la pila
netS (Sta listPalets _ ) = sum (map netP listPalets)
--netS (Sta listPalets _ ) = sumWeights listPalets


holdsS :: Stack -> Palet -> Route -> Bool -- indica si la pila puede aceptar el palet considerando las ciudades en la ruta
holdsS (Sta listPalets capacity) pal rou | freeCellsS (Sta listPalets capacity ) == 0 = False
                                          | not (inRouteR rou (destinationP pal)) = False
                                          | freeCellsS(Sta listPalets capacity) == capacity || destinationP(last listPalets) == destinationP pal = True 
                                          | otherwise = inOrderR rou (destinationP pal) (destinationP(last listPalets)) 

countDestPalets:: [Palet] -> String -> Int  -- cuenta la cantidad de palets con destino en la ciudad indicada
countDestPalets listPalets city | null listPalets = 0
                                | destinationP(last(listPalets)) == city = 1 + countDestPalets (init listPalets) city
                                | otherwise = 0


popS :: Stack -> String -> Stack          -- quita del tope los paletes con destino en la ciudad indicada
popS (Sta listPalets capacity) city = Sta (take(length(listPalets) - countDestPalets listPalets city)  listPalets) capacity
